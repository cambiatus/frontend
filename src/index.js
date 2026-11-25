import * as AbsintheSocket from "@absinthe/socket";
import { Api, JsonRpc } from "eosjs";
import { JsSignatureProvider } from "eosjs/dist/eosjs-jssig";
import { ecc } from "eosjs/dist/eosjs-ecc-migration";
import pdfMake from "pdfmake/build/pdfmake";
import sjcl from "sjcl";
import { Elm } from "./elm/Main.elm";
import configuration from "./scripts/config.js";
import mnemonic from "./scripts/mnemonic.js";
// import registerServiceWorker from './scripts/registerServiceWorker'
import pdfDefinition from "./scripts/pdfDefinition";
import "./styles/main.css";
import pdfFonts from "./vfs_fonts";
import { register as registerCustomElements } from "./customElements/index";
import * as matomo from "./utils/matomo";
import * as memberLocation from "./utils/memberLocation";
import { Socket as PhoenixSocket } from "phoenix";
import * as ElmDebugger from "elm-debug-transformer";
import { serialize } from "eosjs";

// =========================================
// Initial constants
// =========================================

let api = null;
let rpc = null;
const USER_KEY = "bespiral.user";
const LANGUAGE_KEY = "bespiral.language";
const AUTH_TOKEN = "bespiral.auth_token";
const RECENT_SEARCHES = "bespiral.recent_search";
const SELECTED_COMMUNITY_KEY = "bespiral.selected_community";
const PIN_VISIBILITY_KEY = "bespiral.pin_visibility";
const env = process.env.NODE_ENV || "development";
const useSubdomain =
  process.env.USE_SUBDOMAIN === undefined
    ? true
    : process.env.USE_SUBDOMAIN !== "false";
const config = configuration[env];

// =========================================
// App startup
// =========================================

// =========================================
// Client error reporting
// =========================================

// Reports go to our own backend (`POST /api/client_errors`). The endpoint
// always answers 204 and we never retry, so nothing here can break the app.

const CLIENT_ERRORS_URL = "/api/client_errors";
const MAX_BREADCRUMBS = 30;
const MAX_BREADCRUMB_MESSAGE = 300;
const MAX_REPORTS_PER_PAGE_LOAD = 20;
const DUPLICATE_WINDOW_MS = 60 * 1000;
const MAX_BODY_BYTES = 16 * 1024;

const breadcrumbs = [];
const recentReports = new Map();
let reportsSent = 0;

// Keys whose values are secrets (the start-up breadcrumb carries `flags()`, which
// holds the auth token). Their values never leave the browser.
const SECRET_KEYS =
  /^(authtoken|token|privatekey|passphrase|pin|words|signature|signed|password|phrase|mnemonic)$/i;

const scrub = (value, depth = 0) => {
  if (value === null || typeof value !== "object") return value;
  if (depth > 6) return "[too deep]";
  if (Array.isArray(value)) return value.map((item) => scrub(item, depth + 1));

  const clean = {};
  Object.keys(value).forEach((key) => {
    clean[key] = SECRET_KEYS.test(key)
      ? "[removed]"
      : scrub(value[key], depth + 1);
  });
  return clean;
};

/** On production, keeps a breadcrumb in a small in-memory ring buffer that is
  attached to the next report. Needs an object like this:
  { type: 'default' | 'debug' | 'error' | 'info' | 'query',
    category: portName,
    message: 'Something went wrong',
    data: { transactionId },
    localData: { transactionId, passphrase }
    level: 'fatal' | 'error' | 'warning' | 'info' | 'debug'
  }

  Where the localData field is only displayed on development

  On development, just logs that information to the console
*/
const addBreadcrumb = (breadcrumb) => {
  if (env === "development") {
    const { message, ...rest } = breadcrumb;
    console.log("[==== BREADCRUMB]: ", message, rest);
    return;
  }

  // We don't want to keep localData, as it might contain sensitive information
  const { type, category, message, data, level } = breadcrumb;

  breadcrumbs.push({
    type,
    category,
    message: String(message || "").slice(0, MAX_BREADCRUMB_MESSAGE),
    data: scrub(data),
    level,
    timestamp: Date.now(),
  });

  if (breadcrumbs.length > MAX_BREADCRUMBS) {
    breadcrumbs.shift();
  }
};

const currentPathname = () => {
  try {
    return window.location.pathname;
  } catch (_) {
    return null;
  }
};

const storedAccountName = () => {
  try {
    const user = JSON.parse(getItem(USER_KEY));
    return (user && user.accountName) || null;
  } catch (_) {
    return null;
  }
};

const shouldSendReport = ({ level, transaction, message }) => {
  const now = Date.now();
  const key = `${level}|${transaction}|${message}`;

  recentReports.forEach((sentAt, sentKey) => {
    if (now - sentAt >= DUPLICATE_WINDOW_MS) recentReports.delete(sentKey);
  });

  if (reportsSent >= MAX_REPORTS_PER_PAGE_LOAD || recentReports.has(key)) {
    return false;
  }

  reportsSent += 1;
  recentReports.set(key, now);
  return true;
};

const serializeReport = (report) => {
  let body = JSON.stringify(report);

  if (body.length > MAX_BODY_BYTES) {
    body = JSON.stringify({ ...report, breadcrumbs: [] });
  }

  if (body.length > MAX_BODY_BYTES) {
    body = JSON.stringify({ ...report, breadcrumbs: [], contexts: [] });
  }

  return body;
};

const sendReport = (report) => {
  try {
    if (!shouldSendReport(report)) return;

    const body = serializeReport({
      ...report,
      tags: scrub(report.tags),
      contexts: scrub(report.contexts),
      breadcrumbs: breadcrumbs.slice(),
      url: currentPathname(),
      version: process.env.COMMIT,
    });

    fetch(CLIENT_ERRORS_URL, {
      method: "POST",
      keepalive: true,
      headers: { "content-type": "application/json" },
      body,
    }).catch(() => {});
  } catch (_) {
    // Reporting must never break the app
  }
};

/** On production, sends an event to our backend. Needs an object like this:
  { user: eosUsername | null,
    message: 'Something went wrong',
    tags: { 'cambiatus.type': 'eos-transaction' },
    contexts: [{ name: 'Eos transaction', extras: { transactionId: transactionId } }],
    localData: { privateKey },
    transaction: portName,
    level: 'fatal' | 'error' | 'warning' | 'info' | 'debug'
  }

  Where the localData field is only displayed on development

  On development, just logs that information to the console
*/
const logEvent = (event) => {
  addBreadcrumb({
    type: "debug",
    category: "logEvent.start",
    message: "Begin logEvent",
    data: { transaction: event.transaction, eventMessage: event.message },
    localData: event.localData,
    level: "debug",
  });

  if (env === "development") {
    const { message, ...rest } = event;
    console.log("[==== EVENT]: ", message, rest);
  } else {
    const { user, message, tags, contexts, transaction, level } = event;

    sendReport({
      level,
      message,
      transaction,
      account: user || storedAccountName(),
      tags: tags || {},
      contexts: contexts || [],
    });
  }

  addBreadcrumb({
    type: "debug",
    category: "logEvent.end",
    message: "Ended logEvent",
    data: { transaction: event.transaction, eventMessage: event.message },
    localData: event.localData,
    level: "debug",
  });
};

if (env !== "development") {
  // Uncaught errors are not seen by Elm, so we report them from here
  window.addEventListener("error", (event) => {
    const isForeignScript =
      event.filename && !event.filename.startsWith(window.location.origin);
    if (isForeignScript || /ResizeObserver loop/.test(event.message)) return;

    const location = event.filename
      ? ` (${event.filename}:${event.lineno})`
      : "";

    sendReport({
      level: "error",
      message: `${event.message}${location}`,
      transaction: "window.onerror",
      account: storedAccountName(),
      tags: { "cambiatus.language": "javascript" },
      contexts: [],
    });
  });

  window.addEventListener("unhandledrejection", (event) => {
    const reason = event.reason;
    const message = String(
      (reason && reason.message) || reason || "Unhandled promise rejection",
    );
    if (/ResizeObserver loop/.test(message)) return;

    sendReport({
      level: "error",
      message,
      transaction: "window.unhandledrejection",
      account: storedAccountName(),
      tags: { "cambiatus.language": "javascript" },
      contexts: [],
    });
  });
}

const hostnameInfo = () => {
  const environments = ["staging", "demo"];
  const hostnameParts = window.location.hostname.split(".");

  // `true` when on `staging.cambiatus.io`, `demo.cambiatus.io` or `cambiatus.io`
  const isFirstPartEnv =
    environments.includes(hostnameParts[0]) ||
    window.location.hostname === "cambiatus.io";

  if (!isFirstPartEnv) {
    hostnameParts.shift();
  }

  const hostnameEnv =
    hostnameParts[0] === "cambiatus" ? "prod" : hostnameParts[0];
  const subdomain = `.${hostnameParts.join(".")}`;

  return { subdomain, hostnameEnv };
};

const cookieDomain = () => {
  const { subdomain } = hostnameInfo();
  return `domain=${subdomain}`;
};

const cookieKey = (key) => {
  const { hostnameEnv } = hostnameInfo();
  return hostnameEnv === "prod" ? key : `${key}.${hostnameEnv}`;
};

const getItem = (key) => {
  if (useSubdomain) {
    const result = document.cookie.match(
      "(^|[^;]+)\\s*" + cookieKey(key) + "\\s*=\\s*([^;]+)",
    );
    return result ? result.pop() : null;
  }

  return window.localStorage.getItem(cookieKey(key)) || null;
};

const removeItem = (key) => {
  document.cookie = `${cookieKey(key)}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; ${cookieDomain()}; path=/; SameSite=Strict; Secure`;
  window.localStorage.removeItem(cookieKey(key));
};

const setItem = (key, value) => {
  if (useSubdomain) {
    // This is the maximum possible expiration date for some browsers, because
    // they use 32 bits to represent this field (maxExpirationDate === 2^31 - 1).
    // This is equivalent to the date 2038-01-19 04:14:07
    const maxExpirationDate = 2147483647;
    document.cookie = `${cookieKey(key)}=${value}; expires=${new Date(maxExpirationDate * 1000).toUTCString()}; ${cookieDomain()}; path=/; SameSite=Strict; Secure`;
  } else {
    window.localStorage.setItem(cookieKey(key), value);
  }
};

const storedKeys = [
  USER_KEY,
  LANGUAGE_KEY,
  AUTH_TOKEN,
  RECENT_SEARCHES,
  SELECTED_COMMUNITY_KEY,
];

if (useSubdomain) {
  storedKeys.forEach((key) => {
    const localStorageValue = window.localStorage.getItem(key);
    if (localStorageValue !== null) {
      setItem(key, localStorageValue);
      window.localStorage.removeItem(key);
    }
  });
}

pdfMake.vfs = pdfFonts.vfs;
pdfMake.fonts = {
  Nunito: {
    normal: "nunito-regular.ttf",
    bold: "opensans-bold-no-ligatures.ttf",
  },
};
// PhoenixSocket imported at top

if (process.env.NODE_ENV === "development") {
  // Transform `Debug.log` output into nice log object with custom formatter
  // (snippet is taken from https://github.com/MattCheely/elm-app-gen/blob/master/generators/app/templates/parcel/app.js)
  // ElmDebugger imported at top

  const hasFormatterSupport = () => {
    const originalFormatters = window.devtoolsFormatters;
    let supported = false;

    window.devtoolsFormatters = [
      {
        header: function (obj, config) {
          supported = true;
          return null;
        },
        hasBody: function (obj) {},
        body: function (obj, config) {},
      },
    ];
    console.log("elm-debug-transformer: checking for formatter support.", {});
    window.devtoolsFormatters = originalFormatters;
    return supported;
  };

  if (hasFormatterSupport()) {
    ElmDebugger.register();
  } else {
    ElmDebugger.register({ simple_mode: true });
  }
}

function getUserLanguage() {
  const urlParams = new URLSearchParams(window.location.search);

  return (
    urlParams.get("lang") ||
    getItem(LANGUAGE_KEY) ||
    navigator.language ||
    navigator.userLanguage ||
    "en-US"
  );
}

function canReadClipboard() {
  return Boolean(navigator.clipboard) && Boolean(navigator.clipboard.readText);
}

/** Assumes we already have clipboard permissions */
async function readClipboardWithPermission() {
  try {
    const clipboardContent = await navigator.clipboard.readText();
    return { clipboardContent };
  } catch (err) {
    if (err.name && err.name === "NotAllowedError") {
      addBreadcrumb({
        type: "info",
        category: "readClipboardWithPermission",
        message: "User denied permission to read clipboard",
        data: { error: err },
        localData: {},
        level: "info",
      });
    } else {
      logEvent({
        user: null,
        message: "Error when reading clipboard",
        tags: { "cambiatus.kind": "clipboard" },
        contexts: [{ name: "Error details", extras: { error: err } }],
        transaction: "readClipboardWithPermission",
        level: "error",
      });
    }

    return { error: err.message };
  }
}

function flags() {
  const user = JSON.parse(getItem(USER_KEY));
  const accountName = (user && user.accountName) || null;

  return {
    endpoints: config.endpoints,
    language: getUserLanguage(),
    version: process.env.COMMIT,
    accountName,
    authToken: getItem(AUTH_TOKEN),
    logo: config.logo,
    logoMobile: config.logoMobile,
    now: Date.now(),
    allowCommunityCreation: config.allowCommunityCreation,
    tokenContract: config.tokenContract,
    communityContract: config.communityContract,
    canReadClipboard: canReadClipboard(),
    canShare: Boolean(navigator.share),
    useSubdomain,
    selectedCommunity: getItem(SELECTED_COMMUNITY_KEY),
    pinVisibility: JSON.parse(getItem(PIN_VISIBILITY_KEY)) || false,
  };
}

// Start elm app with flags
const app = Elm.Main.init({
  flags: flags(),
});

addBreadcrumb({
  type: "debug",
  category: "elm.start",
  message: "Started Elm app",
  data: { flags: flags() },
  localData: {},
  level: "info",
});

registerCustomElements(app, config, addBreadcrumb);

addBreadcrumb({
  type: "debug",
  category: "elm.start",
  message: "Defined custom elements",
  data: { flags: flags() },
  localData: {},
  level: "info",
});

// Register Service Worker After App
// registerServiceWorker()

// Ports error Reporter
app.ports.addBreadcrumbPort.subscribe(addBreadcrumb);

app.ports.logEvent.subscribe(logEvent);

// =========================================
// EOS / Identity functions
// =========================================

// Initialize RPC without signature provider
rpc = new JsonRpc(config.eosOptions.httpEndpoint, { fetch });

// Initialize API without signature provider initially
api = new Api({
  rpc,
  signatureProvider: new JsSignatureProvider([]),
  textDecoder: new TextDecoder(),
  textEncoder: new TextEncoder(),
});

// STORE LANGUAGE

app.ports.storeLanguage.subscribe(storeLanguage);

function storeLanguage(lang) {
  setItem(LANGUAGE_KEY, lang);
  addBreadcrumb({
    type: "info",
    category: "storeLanguage",
    message: "Stored language",
    data: { language: lang },
    localData: {},
    level: "debug",
  });
}

// STORE RECENT SEARCHES
app.ports.storeRecentSearches.subscribe((query) => {
  setItem(RECENT_SEARCHES, query);
  addBreadcrumb({
    type: "info",
    category: "storeRecentSearches",
    message: "Stored recent searches",
    data: { recentSearches: query },
    localData: {},
    level: "debug",
  });
});

// RETRIEVE RECENT SEARCHES
app.ports.getRecentSearches.subscribe(() => {
  const recentSearches = getItem(RECENT_SEARCHES) || "[]";
  app.ports.gotRecentSearches.send(recentSearches);
  addBreadcrumb({
    type: "info",
    category: "getRecentSearches",
    message: "Got recent searches",
    data: { recentSearches },
    localData: {},
    level: "debug",
  });
});

app.ports.storeAuthToken.subscribe((token) => {
  setItem(AUTH_TOKEN, token);
  addBreadcrumb({
    type: "info",
    category: "storeAuthToken",
    message: "Stored auth token",
    data: {},
    localData: {},
    level: "debug",
  });
});

app.ports.storeSelectedCommunitySymbol.subscribe((symbol) => {
  setItem(SELECTED_COMMUNITY_KEY, symbol);
  joinPresence();
  addBreadcrumb({
    type: "info",
    category: "storeSelectedCommunitySymbol",
    message: "Stored selected community's symbol",
    data: { symbol },
    localData: {},
    level: "debug",
  });
});

app.ports.storePinVisibility.subscribe((pinVisibility) => {
  setItem(PIN_VISIBILITY_KEY, pinVisibility);
  addBreadcrumb({
    type: "info",
    category: "storePinVisibility",
    message: "Stored pin visibility",
    data: { pinVisibility },
    localData: {},
    level: "debug",
  });
});

// STORE PIN

function storePin(data, pin) {
  // encrypt key using PIN
  const hashedKey = ecc.sha256(data.privateKey);
  const encryptedKey = sjcl.encrypt(pin, data.privateKey);

  const storeData = {
    accountName: data.accountName,
    encryptedKey,
    encryptedKeyIntegrityCheck: hashedKey,
  };

  if (data.passphrase) {
    storeData.encryptedPassphrase = sjcl.encrypt(pin, data.passphrase);
  }

  removeItem(USER_KEY);
  setItem(USER_KEY, JSON.stringify(storeData));
  addBreadcrumb({
    type: "info",
    category: "storePin",
    message: "Stored PIN",
    data: {},
    localData: { storeData },
    level: "debug",
  });
}

function logout() {
  removeItem(USER_KEY);
  removeItem(AUTH_TOKEN);
  leavePresence();

  addBreadcrumb({
    type: "info",
    category: "logout",
    message: "User logged out",
    data: {},
    localData: {},
    level: "info",
  });
}

function downloadPdf(accountName, passphrase) {
  addBreadcrumb({
    type: "info",
    category: "downloadPdf",
    message: "Started downloading pdf",
    data: { accountName },
    localData: {},
    level: "debug",
  });

  const definition = pdfDefinition(passphrase);
  const pdf = pdfMake.createPdf(definition);

  pdf.download(accountName + "_cambiatus.pdf");

  addBreadcrumb({
    type: "info",
    category: "downloadPdf",
    message: "Finished downloading pdf",
    data: { accountName },
    localData: {},
    level: "debug",
  });

  return { isDownloaded: true };
}

app.ports.javascriptOutPort.subscribe(async (arg) => {
  addBreadcrumb({
    type: "info",
    category: arg.data.name,
    message: "Port handler started",
    data: {},
    localData: arg,
    level: "debug",
  });

  const defaultResponse = {
    address: arg.responseAddress,
    addressData: arg.responseData,
  };

  const portResponse = await handleJavascriptPort(arg);
  addBreadcrumb({
    type: "info",
    category: arg.data.name,
    message: "Got port handler response",
    data: {},
    localData: portResponse,
    level: "debug",
  });

  const response = { ...defaultResponse, ...portResponse };

  if (response.error) {
    addBreadcrumb({
      type: "info",
      category: arg.data.name,
      message: "Port handler failed",
      data: { error: response.error },
      localData: { response },
      level: "error",
    });
  } else {
    addBreadcrumb({
      type: "info",
      category: arg.data.name,
      message: "Port handler finished without errors",
      data: {},
      localData: { response },
      level: "info",
    });
  }

  // Only send data through port if there is a portResponse, and the port is not a subscription
  if (Object.keys(portResponse).length !== 0 && !response.isSubscription) {
    app.ports.javascriptInPort.send(response);
  }
});

// All notifiers for GraphQL subscriptions through absinthe socket
let newCommunitySubscription = null;
let transferSubscription = null;
let notificationSubscription = null;
let highlightedNewsSubscription = null;

let absintheSocket = AbsintheSocket.create(
  new PhoenixSocket(config.endpoints.socket),
);

app.ports.createAbsintheSocket.subscribe((token) => {
  const oldAbsintheSocket = absintheSocket;

  absintheSocket = AbsintheSocket.create(
    new PhoenixSocket(
      `${config.endpoints.socket}/websocket?Authorization=Bearer ${token}&vsn=2.0.0`,
    ),
  );

  const resubscribe = (subscription) => {
    if (subscription === null) {
      return;
    }

    const { notifier, handlers, operation } = subscription;

    AbsintheSocket.cancel(oldAbsintheSocket, notifier);

    const newNotifier = AbsintheSocket.send(absintheSocket, {
      operation,
      variables: {},
    });

    AbsintheSocket.observe(absintheSocket, newNotifier, handlers);
  };

  resubscribe(newCommunitySubscription);
  resubscribe(transferSubscription);
  resubscribe(notificationSubscription);
  resubscribe(highlightedNewsSubscription);

  joinPresence();
});

// Tells the backend this member is online in the Elm app, so LiveView can show it. The
// channel rides the socket Absinthe already uses (its token authenticates the join) and
// is left on logout; a new socket (token change) or community switch rejoins.
let presenceChannel = null;

function leavePresence() {
  if (presenceChannel !== null) {
    presenceChannel.leave();
    presenceChannel = null;
  }
}

function joinPresence() {
  leavePresence();

  const symbol = getItem(SELECTED_COMMUNITY_KEY);
  if (!getItem(AUTH_TOKEN) || !symbol) {
    return;
  }

  presenceChannel = absintheSocket.phoenixSocket.channel("elm_presence", {
    symbol,
  });
  // A refused join (not a member) must not be retried forever
  presenceChannel.join().receive("error", leavePresence);
}

async function handleJavascriptPort(arg) {
  switch (arg.data.name) {
    case "checkAccountAvailability": {
      return rpc
        .get_account(arg.data.account)
        .then((_) => ({ isAvailable: false, error: "account not available" }))
        .catch((e) => {
          // Invalid name exception
          if (e.json && e.json.error && e.json.error.code === 3010001) {
            return { isAvailable: false, error: e.message };
          } else if (e.message && e.message.includes("unknown key")) {
            return { isAvailable: true };
          } else {
            return { isAvailable: true };
          }
        });
    }
    case "generateKeys": {
      const userLang = getUserLanguage();
      const [randomWords, hexRandomWords] = mnemonic.generateRandom(userLang);
      const privateKey = mnemonic.seedPrivate(hexRandomWords);
      const publicKey = ecc.privateToPublic(privateKey);

      return {
        data: {
          ownerKey: publicKey,
          activeKey: publicKey,
          accountName: arg.data.account,
          words: randomWords,
          privateKey,
        },
      };
    }
    case "getAccountFrom12Words": {
      const { passphrase } = arg.data;
      const privateKey = mnemonic.seedPrivate(mnemonic.toSeedHex(passphrase));

      if (!ecc.isValidPrivate(privateKey)) {
        return { error: "error.invalidKey" };
      } else {
        try {
          const publicKey = ecc.privateToPublic(privateKey);
          const accounts = await rpc.history_get_key_accounts(publicKey);

          if (
            !accounts ||
            !accounts.account_names ||
            accounts.account_names.length === 0
          ) {
            return { error: "error.accountNotFound" };
          } else {
            const [accountName] = accounts.account_names;
            return { accountName, privateKey };
          }
        } catch (err) {
          logEvent({
            user: null,
            message: "Login: could not look up the account for the 12 words",
            tags: { "cambiatus.kind": "auth" },
            contexts: [
              {
                name: "Error details",
                extras: { error: String((err && err.message) || err) },
              },
            ],
            transaction: "getAccountFromKey",
            level: "error",
          });

          return { error: "error.unknown" };
        }
      }
    }
    case "signString": {
      const { input, privateKey } = arg.data;

      try {
        const signed = ecc.sign(input, privateKey);

        return { signed };
      } catch (err) {
        // Never include the error message or the key: they could echo the key
        logEvent({
          user: null,
          message: "Login: signing the auth phrase failed",
          tags: { "cambiatus.kind": "auth" },
          contexts: [
            { name: "Error details", extras: { errorName: err && err.name } },
          ],
          transaction: "signString",
          level: "error",
        });

        return { error: "error.unknown" };
      }
    }
    case "login": {
      const { privateKey, passphrase, accountName, pin } = arg.data;

      logout();

      storePin({ accountName, passphrase, privateKey }, pin);

      // Save credentials to EOS - create new API instance with signature provider
      const signatureProvider = new JsSignatureProvider([privateKey]);
      api = new Api({
        rpc,
        signatureProvider,
        textDecoder: new TextDecoder(),
        textEncoder: new TextEncoder(),
      });

      addBreadcrumb({
        type: "debug",
        category: "login",
        message: "Saved credentials to EOS",
        data: {},
        localData: { accountName },
        level: "debug",
      });

      return {};
    }
    case "changePin": {
      const userStorage = JSON.parse(getItem(USER_KEY));
      const currentPin = arg.data.currentPin;
      const newPin = arg.data.newPin;
      const decryptedKey = sjcl.decrypt(currentPin, userStorage.encryptedKey);

      const data = {
        accountName: userStorage.accountName,
        privateKey: decryptedKey,
      };

      data.passphrase = sjcl.decrypt(
        currentPin,
        userStorage.encryptedPassphrase,
      );

      storePin(data, newPin);

      return { accountName: arg.data.accountName, privateKey: decryptedKey };
    }
    case "getPrivateKey": {
      const user = JSON.parse(getItem(USER_KEY));
      const pin = arg.data.pin;
      // If private key and accountName are stored in localStorage
      const isUserLoggedIn = user && user.encryptedKey && user.accountName;
      if (!isUserLoggedIn) {
        return { error: "error.unavailablePin" };
      } else {
        try {
          const decryptedKey = sjcl.decrypt(pin, user.encryptedKey);

          // Create new API instance with signature provider
          const signatureProvider = new JsSignatureProvider([decryptedKey]);
          api = new Api({
            rpc,
            signatureProvider,
            textDecoder: new TextDecoder(),
            textEncoder: new TextEncoder(),
          });

          addBreadcrumb({
            type: "debug",
            category: "getPrivateKey",
            message: "Saved credentials to EOS",
            data: {},
            localData: {},
            level: "debug",
          });

          addBreadcrumb({
            type: "info",
            category: "getPrivateKey",
            message: "Logged user with PIN",
            data: { accountName: user.accountName },
            localData: {},
            level: "info",
          });

          return { accountName: user.accountName, privateKey: decryptedKey };
        } catch (e) {
          return { error: "error.invalidPin" };
        }
      }
    }
    case "eosTransaction": {
      addBreadcrumb({
        type: "debug",
        category: "eosTransaction",
        message: "Started pushing transaction to EOS",
        data: {},
        localData: { actions: arg.data.actions },
        level: "info",
      });

      return api
        .transact(
          { actions: arg.data.actions },
          {
            blocksBehind: 3,
            expireSeconds: 30,
          },
        )
        .then((res) => {
          addBreadcrumb({
            type: "debug",
            category: "eosTransaction",
            message: "Finished pushing transaction to EOS without errors",
            data: { transactionId: res.transaction_id },
            localData: {},
            level: "info",
          });

          return { transactionId: res.transaction_id };
        })
        .catch((error) => {
          let errorMessage;
          try {
            if (error.json && error.json.error && error.json.error.details) {
              errorMessage = `[EOS] ${error.json.error.details[0].message}`;
            } else if (error.message) {
              errorMessage = `[EOS] ${error.message}`;
            } else {
              errorMessage = "Got an error when pushing transaction to EOS";
            }
          } catch {
            errorMessage = "Got an error when pushing transaction to EOS";
          }

          const response = { error };

          logEvent({
            user: null,
            message: errorMessage,
            tags: { "cambiatus.type": "eos-transaction" },
            contexts: [
              {
                name: "Eos transaction",
                extras: {
                  sent: arg.data,
                  response,
                  error,
                },
              },
            ],
            localData: {},
            transaction: "eosTransaction",
            level: "error",
          });

          return response;
        });
    }
    case "logout": {
      logout();

      return {};
    }
    case "downloadAuthPdfFromRegistration": {
      const { accountName, passphrase } = arg.data;
      return downloadPdf(accountName, passphrase);
    }
    case "downloadAuthPdfFromProfile": {
      const store = JSON.parse(getItem(USER_KEY));
      const pin = arg.data.pin;

      const decryptedPassphrase = sjcl.decrypt(pin, store.encryptedPassphrase);
      return downloadPdf(store.accountName, decryptedPassphrase);
    }
    case "accountNameToUint64": {
      // In eosjs v22, we need to use the serialize module
      // serialize imported at top
      return {
        uint64name: serialize.nameToUint64(arg.data.accountName),
      };
    }
    case "scrollIntoView": {
      // We might be creating the element and scrolling to it at the same time.
      // If we don't use setTimeout, we might try to scroll to the element before it's created, which produces a runtime error
      setTimeout(() => {
        document.getElementById(arg.data.id).scrollIntoView(true);
      }, 0);

      return {};
    }
    case "smoothHorizontalScroll": {
      const { containerId, targetId } = arg.data;

      const targetLeft = document
        .getElementById(targetId)
        .getBoundingClientRect().left;
      const container = document.getElementById(containerId);
      const offset = targetLeft - container.getBoundingClientRect().left;
      container.scrollTo({
        left: container.scrollLeft + offset,
        behavior: "smooth",
      });

      return {};
    }
    case "subscribeToNewCommunity": {
      // Cancel existing notifier
      if (newCommunitySubscription && newCommunitySubscription.notifier) {
        AbsintheSocket.cancel(
          absintheSocket,
          newCommunitySubscription.notifier,
        );
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {},
      });

      const onStart = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToNewCommunity",
          message: "Started listening to new communities",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: "starting",
        };
        app.ports.javascriptInPort.send(response);
      };

      const onAbort = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToNewCommunity",
          message: "Aborted listening to new communities",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onCancel = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToNewCommunity",
          message: "Cancelled listening to new communities",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onError = (data) => {
        addBreadcrumb({
          type: "error",
          category: "subscribeToNewCommunity",
          message: "Error when listening to new communities",
          data: {},
          localData: { data },
          level: "error",
        });
      };

      const onResult = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToNewCommunity",
          message: "Got a new community result",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: "responded",
        };
        app.ports.javascriptInPort.send(response);
      };

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult,
      };

      newCommunitySubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription,
      };

      AbsintheSocket.observe(absintheSocket, notifier, handlers);

      return { isSubscription: true };
    }
    case "subscribeToTransfer": {
      // Cancel existing notifier
      if (transferSubscription && transferSubscription.notifier) {
        AbsintheSocket.cancel(absintheSocket, transferSubscription.notifier);
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {},
      });

      const onStart = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToTransfer",
          message: "Started listening for transfer",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: "starting",
        };
        app.ports.javascriptInPort.send(response);
      };

      const onAbort = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToTransfer",
          message: "Aborted listening for transfer",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onCancel = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToTransfer",
          message: "Cancelled listening for transfer",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onError = (data) => {
        addBreadcrumb({
          type: "error",
          category: "subscribeToTransfer",
          message: "Error when listening for transfer",
          data: {},
          localData: { data },
          level: "error",
        });
      };

      const onResult = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToTransfer",
          message: "Got transfer result",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: "responded",
          data,
        };
        app.ports.javascriptInPort.send(response);
      };

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult,
      };

      transferSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription,
      };

      AbsintheSocket.observe(absintheSocket, notifier, handlers);

      return { isSubscription: true };
    }
    case "subscribeToUnreadCount": {
      // Cancel existing notifier
      if (notificationSubscription && notificationSubscription.notifier) {
        AbsintheSocket.cancel(
          absintheSocket,
          notificationSubscription.notifier,
        );
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {},
      });

      const onStart = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToUnreadCount",
          message: "Started listening for unread notifications",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onAbort = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToUnreadCount",
          message: "Aborted listening for unread notifications",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onCancel = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToUnreadCount",
          message: "Cancelled listening for unread notifications",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onError = (data) => {
        addBreadcrumb({
          type: "error",
          category: "subscribeToUnreadCount",
          message: "Error when listening for unread notifications",
          data: {},
          localData: { data },
          level: "error",
        });
      };

      const onResult = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToUnreadCount",
          message: "Got a result when listening for unread notifications",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          meta: data,
        };
        app.ports.javascriptInPort.send(response);
      };

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult,
      };

      notificationSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription,
      };

      AbsintheSocket.observe(absintheSocket, notifier, handlers);

      return { isSubscription: true };
    }
    case "subscribeToHighlightedNewsChanged": {
      // Cancel existing notifier
      if (highlightedNewsSubscription && highlightedNewsSubscription.notifier) {
        AbsintheSocket.cancel(
          absintheSocket,
          highlightedNewsSubscription.notifier,
        );
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {},
      });

      const onStart = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToHighlightedNewsChanged",
          message: "Started listening for highlighted news changes",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onAbort = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToHighlightedNewsChanged",
          message: "Aborted listening for highlighted news changes",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onCancel = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToHighlightedNewsChanged",
          message: "Cancelled listening for highlighted news changes",
          data: {},
          localData: { data },
          level: "info",
        });
      };

      const onError = (data) => {
        addBreadcrumb({
          type: "error",
          category: "subscribeToHighlightedNewsChanged",
          message: "Error listening for highlighted news changes",
          data: {},
          localData: { data },
          level: "error",
        });
      };

      const onResult = (data) => {
        addBreadcrumb({
          type: "info",
          category: "subscribeToHighlightedNewsChanged",
          message: "Got a result when listening for highlighted news changes",
          data: {},
          localData: { data },
          level: "info",
        });

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          meta: data,
        };
        app.ports.javascriptInPort.send(response);
      };

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult,
      };

      highlightedNewsSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription,
      };

      AbsintheSocket.observe(absintheSocket, notifier, handlers);

      return { isSubscription: true };
    }
    case "copyToClipboard": {
      // We might need to want to change the dom before copying contents of the input
      await new Promise(function (resolve, reject) {
        window.setTimeout(async () => {
          const element = document.getElementById(arg.data.id);
          if (!element) {
            reject(new Error("Element not found"));
            return;
          }

          // The clipboard API is not supported in all browsers
          if (
            navigator.clipboard &&
            navigator.clipboard.writeText &&
            element.value
          ) {
            await navigator.clipboard.writeText(element.value);

            resolve();
            return;
          }

          element.select();
          document.execCommand("copy");

          resolve();
        }, 0);
      });

      return { copied: true };
    }
    case "memberLocation": {
      return await memberLocation.locate();
    }
    case "dismissAddressNote": {
      memberLocation.dismissAddressNote();
      return {};
    }
    case "readClipboard": {
      if (canReadClipboard()) {
        try {
          const permissionStatus = await navigator.permissions.query({
            name: "clipboard-read",
            allowWithoutGesture: true,
          });

          switch (permissionStatus.state) {
            case "denied": {
              addBreadcrumb({
                type: "info",
                category: "readClipboard",
                message: "Checked for clipboard access, and it's denied",
                data: {},
                localData: {},
                level: "info",
              });

              // We can't request for permission, the user needs to do it manually
              return { isDenied: true };
            }
            case "granted": {
              addBreadcrumb({
                type: "info",
                category: "readClipboard",
                message: "Checked for clipboard access, and it's granted",
                data: {},
                localData: {},
                level: "info",
              });

              return readClipboardWithPermission();
            }
            case "prompt": {
              addBreadcrumb({
                type: "debug",
                category: "readClipboard",
                message: "Prompting user for clipboard access",
                data: {},
                localData: {},
                level: "debug",
              });

              try {
                const clipboardContent = await navigator.clipboard.readText();
                addBreadcrumb({
                  type: "info",
                  category: "readClipboard",
                  message: "User granted access to clipboard",
                  data: {},
                  localData: { clipboardContent },
                  level: "info",
                });

                return { clipboardContent };
              } catch (err) {
                addBreadcrumb({
                  type: "info",
                  category: "readClipboard",
                  message: "User denied access to clipboard",
                  data: {},
                  localData: { error: err },
                  level: "info",
                });

                return { isDenied: true };
              }
            }
            default: {
              return { error: "permissionStatus state unkown" };
            }
          }
        } catch (permissionError) {
          addBreadcrumb({
            type: "info",
            category: "readClipboard",
            message:
              "The permissions API is not supported by the user's browser",
            data: { permissionError },
            localData: {},
            level: "info",
          });

          return readClipboardWithPermission();
        }
      } else {
        addBreadcrumb({
          type: "info",
          category: "readClipboard",
          message:
            "clipboard.readText() is not supported by the user's browser",
          data: {},
          localData: {},
          level: "info",
        });

        return { notSupported: true };
      }
    }
    case "share": {
      const { title, text, url } = arg.data;

      try {
        await navigator.share({ title, text, url });
        return {};
      } catch (err) {
        return { error: err };
      }
    }
    case "setFavicon": {
      const { favicon } = arg.data;
      document.head.querySelectorAll("link[rel*=icon]").forEach((icon) => {
        icon.href = favicon;
      });

      return {};
    }
    case "addClassToDocument": {
      const { className } = arg.data;

      document.querySelector("html").classList.add(className);
      document.querySelector("body").classList.add(className);

      return {};
    }
    case "removeClassFromDocument": {
      const { className } = arg.data;

      document.querySelector("html").classList.remove(className);
      document.querySelector("body").classList.remove(className);

      return {};
    }
    case "getBip39": {
      const normalize = (wordlist) => {
        return wordlist.map((word) => word.normalize());
      };

      return {
        english: normalize(mnemonic.wordlists.english),
        portuguese: normalize(mnemonic.wordlists.portuguese),
        spanish: normalize(mnemonic.wordlists.spanish),
      };
    }
    case "addMatomoScript": {
      matomo.addScript();

      return {};
    }
    default: {
      return { error: `No treatment found for Elm port ${arg.data.name}` };
    }
  }
}
