import * as AbsintheSocket from '@absinthe/socket'
import * as Sentry from '@sentry/browser'
import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import pdfMake from 'pdfmake/build/pdfmake'
import sjcl from 'sjcl'
import { Elm } from './elm/Main.elm'
import configuration from './scripts/config.js'
import mnemonic from './scripts/mnemonic.js'
// import registerServiceWorker from './scripts/registerServiceWorker'
import pdfDefinition from './scripts/pdfDefinition'
import './styles/main.css'
import pdfFonts from './vfs_fonts'
import { register as registerCustomElements } from './customElements/index'
import * as bip39 from 'bip39'
import * as matomo from './utils/matomo'

// =========================================
// Initial constants
// =========================================

let eos = null
const USER_KEY = 'bespiral.user'
const LANGUAGE_KEY = 'bespiral.language'
const AUTH_TOKEN = 'bespiral.auth_token'
const RECENT_SEARCHES = 'bespiral.recent_search'
const SELECTED_COMMUNITY_KEY = 'bespiral.selected_community'
const PIN_VISIBILITY_KEY = 'bespiral.pin_visibility'
const HAS_SEEN_SPONSOR_MODAL_KEY = 'bespiral.has_seen_sponsor_modal'
const env = process.env.NODE_ENV || 'development'
const useSubdomain = process.env.USE_SUBDOMAIN === undefined ? true : process.env.USE_SUBDOMAIN !== 'false'
const config = configuration[env]

// =========================================
// App startup
// =========================================

if (env !== 'development') {
  // Init Sentry as soon as possible so it starts recording events and breadcrumbs
  // automatically
  Sentry.init({
    dsn: 'https://535b151f7b8c48f8a7307b9bc83ebeba@sentry.io/1480468',
    environment: env,
    beforeBreadcrumb (breadcrumb, hint) {
      // We have a limited amount of breadcrumbs, and these aren't super useful
      // to us, so we just don't include them
      const unusedCategories = ['ui.click', 'ui.input']

      return unusedCategories.includes(breadcrumb.category) ? null : breadcrumb
    }
  })

  Sentry.setTag('cambiatus.version', process.env.COMMIT)
}

/** On production, adds a breadcrumb to sentry. Needs an object like this:
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
  if (env === 'development') {
    const { message, ...rest } = breadcrumb
    console.log('[==== BREADCRUMB]: ', message, rest)
    return
  }

  // We don't want to send localData to sentry, as it might contain sensitive
  // information
  const { localData, ...rest } = breadcrumb

  Sentry.addBreadcrumb(rest)
}

/** On production, sends an event to Sentry. Needs an object like this:
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
    type: 'debug',
    category: 'logEvent.start',
    message: 'Begin logEvent',
    data: { transaction: event.transaction, eventMessage: event.message },
    localData: event.localData,
    level: 'debug'
  })

  if (env === 'development') {
    const { message, ...rest } = event
    console.log('[==== EVENT]: ', message, rest)
  } else {
    const { user, message, tags, contexts, transaction, level } = event

    Sentry.withScope((scope) => {
      if (user !== null) {
        scope.setUser({ username: user })
      } else {
        const user = JSON.parse(getItem(USER_KEY))
        const accountName = (user && user.accountName) || null

        if (accountName !== null) {
          scope.setUser({ username: accountName })
        }
      }
      // If the error comes from Elm, this key will be overwritten
      scope.setTag('cambiatus.language', 'javascript')
      scope.setTags(tags)
      scope.setTransaction(transaction)
      contexts.forEach((context) => {
        scope.setContext(context.name, context.extras)
      })
      scope.setLevel(level)

      Sentry.captureMessage(message)
    })
  }

  addBreadcrumb({
    type: 'debug',
    category: 'logEvent.end',
    message: 'Ended logEvent',
    data: { transaction: event.transaction, eventMessage: event.message },
    localData: event.localData,
    level: 'debug'
  })
}

const hostnameInfo = () => {
  const environments = ['staging', 'demo']
  let hostnameParts = window.location.hostname.split('.')

  // `true` when on `staging.cambiatus.io`, `demo.cambiatus.io` or `cambiatus.io`
  const isFirstPartEnv = environments.includes(hostnameParts[0]) || window.location.hostname === 'cambiatus.io'

  if (!isFirstPartEnv) {
    hostnameParts.shift()
  }

  const hostnameEnv = hostnameParts[0] === 'cambiatus' ? 'prod' : hostnameParts[0]
  const subdomain = `.${hostnameParts.join('.')}`

  return { subdomain, hostnameEnv }
}

const cookieDomain = () => {
  const { subdomain } = hostnameInfo()
  return `domain=${subdomain}`
}

const cookieKey = (key) => {
  const { hostnameEnv } = hostnameInfo()
  return hostnameEnv === 'prod' ? key : `${key}.${hostnameEnv}`
}

const getItem = (key) => {
  if (useSubdomain) {
    const result = document.cookie.match('(^|[^;]+)\\s*' + cookieKey(key) + '\\s*=\\s*([^;]+)')
    return result ? result.pop() : null
  }

  return window.localStorage.getItem(cookieKey(key)) || null
}

const removeItem = (key) => {
  document.cookie = `${cookieKey(key)}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; ${cookieDomain()}; path=/; SameSite=Strict; Secure`
  window.localStorage.removeItem(cookieKey(key))
}

const setItem = (key, value) => {
  if (useSubdomain) {
    // This is the maximum possible expiration date for some browsers, because
    // they use 32 bits to represent this field (maxExpirationDate === 2^31 - 1).
    // This is equivalent to the date 2038-01-19 04:14:07
    const maxExpirationDate = 2147483647
    document.cookie = `${cookieKey(key)}=${value}; expires=${new Date(maxExpirationDate * 1000).toUTCString()}; ${cookieDomain()}; path=/; SameSite=Strict; Secure`
  } else {
    window.localStorage.setItem(cookieKey(key), value)
  }
}

const storedKeys = [USER_KEY, LANGUAGE_KEY, AUTH_TOKEN, RECENT_SEARCHES, SELECTED_COMMUNITY_KEY]

if (useSubdomain) {
  storedKeys.forEach((key) => {
    const localStorageValue = window.localStorage.getItem(key)
    if (localStorageValue !== null) {
      setItem(key, localStorageValue)
      window.localStorage.removeItem(key)
    }
  })
}

pdfMake.vfs = pdfFonts.pdfMake.vfs
pdfMake.fonts = {
  Nunito: {
    normal: 'nunito-regular.ttf',
    bold: 'opensans-bold-no-ligatures.ttf'
  }
}
const { Socket: PhoenixSocket } = require('phoenix')

if (process.env.NODE_ENV === 'development') {
  // Transform `Debug.log` output into nice log object with custom formatter
  // (snippet is taken from https://github.com/MattCheely/elm-app-gen/blob/master/generators/app/templates/parcel/app.js)
  const ElmDebugger = require('elm-debug-transformer')

  const hasFormatterSupport = () => {
    const originalFormatters = window.devtoolsFormatters
    let supported = false

    window.devtoolsFormatters = [
      {
        header: function (obj, config) {
          supported = true
          return null
        },
        hasBody: function (obj) { },
        body: function (obj, config) { }
      }
    ]
    console.log('elm-debug-transformer: checking for formatter support.', {})
    window.devtoolsFormatters = originalFormatters
    return supported
  }

  if (hasFormatterSupport()) {
    ElmDebugger.register()
  } else {
    ElmDebugger.register({ simple_mode: true })
  }
}

function getUserLanguage () {
  const urlParams = new URLSearchParams(window.location.search)

  return (
    urlParams.get('lang') ||
    getItem(LANGUAGE_KEY) ||
    navigator.language ||
    navigator.userLanguage ||
    'en-US'
  )
}

function canReadClipboard () {
  return Boolean(navigator.clipboard) &&
    Boolean(navigator.clipboard.readText)
}

/** Assumes we already have clipboard permissions */
async function readClipboardWithPermission () {
  try {
    const clipboardContent = await navigator.clipboard.readText()
    return { clipboardContent }
  } catch (err) {
    if (err.name && err.name === 'NotAllowedError') {
      addBreadcrumb({
        type: 'info',
        category: 'readClipboardWithPermission',
        message: 'User denied permission to read clipboard',
        data: { error: err },
        localData: {},
        level: 'info'
      })
    } else {
      logEvent({
        user: null,
        message: 'Error when reading clipboard',
        tags: { 'cambiatus.kind': 'clipboard' },
        contexts: [{ name: 'Error details', extras: { error: err } }],
        transaction: 'readClipboardWithPermission',
        level: 'error'
      })
    }

    return { error: err.message }
  }
}

function flags () {
  const user = JSON.parse(getItem(USER_KEY))
  const accountName = (user && user.accountName) || null

  if (env !== 'development' && accountName !== null) {
    Sentry.configureScope((scope) => { scope.setUser({ username: accountName }) })
  }

  return {
    endpoints: config.endpoints,
    language: getUserLanguage(),
    version: process.env.COMMIT,
    accountName: accountName,
    authToken: getItem(AUTH_TOKEN),
    logo: config.logo,
    logoMobile: config.logoMobile,
    now: Date.now(),
    allowCommunityCreation: config.allowCommunityCreation,
    tokenContract: config.tokenContract,
    communityContract: config.communityContract,
    canReadClipboard: canReadClipboard(),
    canShare: Boolean(navigator.share),
    useSubdomain: useSubdomain,
    selectedCommunity: getItem(SELECTED_COMMUNITY_KEY),
    pinVisibility: JSON.parse(getItem(PIN_VISIBILITY_KEY)) || false,
    hasSeenSponsorModal: JSON.parse(getItem(HAS_SEEN_SPONSOR_MODAL_KEY)) || false
  }
}

// Start elm app with flags
const app = Elm.Main.init({
  flags: flags()
})

addBreadcrumb({
  type: 'debug',
  category: 'elm.start',
  message: 'Started Elm app',
  data: { flags: flags() },
  localData: {},
  level: 'info'
})

registerCustomElements(app, config, addBreadcrumb)

addBreadcrumb({
  type: 'debug',
  category: 'elm.start',
  message: 'Defined custom elements',
  data: { flags: flags() },
  localData: {},
  level: 'info'
})

// Register Service Worker After App
// registerServiceWorker()

// Ports error Reporter
app.ports.addBreadcrumbPort.subscribe(addBreadcrumb)

app.ports.logEvent.subscribe(logEvent)

// =========================================
// EOS / Identity functions
// =========================================

eos = Eos(config.eosOptions)

// STORE LANGUAGE

app.ports.storeLanguage.subscribe(storeLanguage)

function storeLanguage (lang) {
  setItem(LANGUAGE_KEY, lang)
  addBreadcrumb({
    type: 'info',
    category: 'storeLanguage',
    message: 'Stored language',
    data: { language: lang },
    localData: {},
    level: 'debug'
  })
}

// STORE RECENT SEARCHES
app.ports.storeRecentSearches.subscribe(query => {
  setItem(RECENT_SEARCHES, query)
  addBreadcrumb({
    type: 'info',
    category: 'storeRecentSearches',
    message: 'Stored recent searches',
    data: { recentSearches: query },
    localData: {},
    level: 'debug'
  })
})

// RETRIEVE RECENT SEARCHES
app.ports.getRecentSearches.subscribe(() => {
  const recentSearches = getItem(RECENT_SEARCHES) || '[]'
  app.ports.gotRecentSearches.send(recentSearches)
  addBreadcrumb({
    type: 'info',
    category: 'getRecentSearches',
    message: 'Got recent searches',
    data: { recentSearches: recentSearches },
    localData: {},
    level: 'debug'
  })
})

app.ports.storeAuthToken.subscribe(token => {
  setItem(AUTH_TOKEN, token)
  addBreadcrumb({
    type: 'info',
    category: 'storeAuthToken',
    message: 'Stored auth token',
    data: {},
    localData: {},
    level: 'debug'
  })
})

app.ports.storeSelectedCommunitySymbol.subscribe(symbol => {
  setItem(SELECTED_COMMUNITY_KEY, symbol)
  addBreadcrumb({
    type: 'info',
    category: 'storeSelectedCommunitySymbol',
    message: 'Stored selected community\'s symbol',
    data: { symbol },
    localData: {},
    level: 'debug'
  })
})

app.ports.storePinVisibility.subscribe(pinVisibility => {
  setItem(PIN_VISIBILITY_KEY, pinVisibility)
  addBreadcrumb({
    type: 'info',
    category: 'storePinVisibility',
    message: 'Stored pin visibility',
    data: { pinVisibility },
    localData: {},
    level: 'debug'
  })
})

app.ports.storeHasSeenSponsorModal.subscribe(hasSeenSponsorModal => {
  setItem(HAS_SEEN_SPONSOR_MODAL_KEY, hasSeenSponsorModal)
  addBreadcrumb({
    type: 'info',
    category: 'storeHasSeenSponsorModal',
    message: 'Stored whether or not the user has seen the sponsor modal',
    data: { hasSeenSponsorModal },
    localData: {},
    level: 'debug'
  })
})

// STORE PIN

function storePin (data, pin) {
  // encrypt key using PIN
  const hashedKey = ecc.sha256(data.privateKey)
  const encryptedKey = sjcl.encrypt(pin, data.privateKey)

  const storeData = {
    accountName: data.accountName,
    encryptedKey: encryptedKey,
    encryptedKeyIntegrityCheck: hashedKey
  }

  if (data.passphrase) {
    storeData.encryptedPassphrase = sjcl.encrypt(pin, data.passphrase)
  }

  removeItem(USER_KEY)
  setItem(USER_KEY, JSON.stringify(storeData))
  addBreadcrumb({
    type: 'info',
    category: 'storePin',
    message: 'Stored PIN',
    data: {},
    localData: { storeData },
    level: 'debug'
  })
}

function logout () {
  removeItem(USER_KEY)
  removeItem(AUTH_TOKEN)

  addBreadcrumb({
    type: 'info',
    category: 'logout',
    message: 'User logged out',
    data: {},
    localData: {},
    level: 'info'
  })
  if (env !== 'development') {
    Sentry.configureScope((scope) => { scope.setUser(null) })
  }
}

function downloadPdf (accountName, passphrase) {
  addBreadcrumb({
    type: 'info',
    category: 'downloadPdf',
    message: 'Started downloading pdf',
    data: { accountName },
    localData: {},
    level: 'debug'
  })

  const definition = pdfDefinition(passphrase)
  const pdf = pdfMake.createPdf(definition)

  pdf.download(accountName + '_cambiatus.pdf')

  addBreadcrumb({
    type: 'info',
    category: 'downloadPdf',
    message: 'Finished downloading pdf',
    data: { accountName },
    localData: {},
    level: 'debug'
  })

  return { isDownloaded: true }
}

app.ports.javascriptOutPort.subscribe(async (arg) => {
  addBreadcrumb({
    type: 'info',
    category: arg.data.name,
    message: 'Port handler started',
    data: {},
    localData: arg,
    level: 'debug'
  })

  const defaultResponse = {
    address: arg.responseAddress,
    addressData: arg.responseData
  }

  const portResponse = await handleJavascriptPort(arg)
  addBreadcrumb({
    type: 'info',
    category: arg.data.name,
    message: 'Got port handler response',
    data: {},
    localData: portResponse,
    level: 'debug'
  })

  const response = { ...defaultResponse, ...portResponse }

  if (response.error) {
    addBreadcrumb({
      type: 'info',
      category: arg.data.name,
      message: 'Port handler failed',
      data: { error: response.error },
      localData: { response },
      level: 'error'
    })
  } else {
    addBreadcrumb({
      type: 'info',
      category: arg.data.name,
      message: 'Port handler finished without errors',
      data: {},
      localData: { response },
      level: 'info'
    })
  }

  // Only send data through port if there is a portResponse, and the port is not a subscription
  if (Object.keys(portResponse).length !== 0 && !response.isSubscription) {
    app.ports.javascriptInPort.send(response)
  }
})

// All notifiers for GraphQL subscriptions through absinthe socket
let newCommunitySubscription = null
let transferSubscription = null
let notificationSubscription = null
let highlightedNewsSubscription = null

let absintheSocket = AbsintheSocket.create(new PhoenixSocket(config.endpoints.socket))

app.ports.createAbsintheSocket.subscribe((token) => {
  const oldAbsintheSocket = absintheSocket

  absintheSocket = AbsintheSocket.create(new PhoenixSocket(`${config.endpoints.socket}/websocket?Authorization=Bearer ${token}&vsn=2.0.0`))

  const resubscribe = (subscription) => {
    if (subscription === null) {
      return
    }

    const { notifier, handlers, operation } = subscription

    AbsintheSocket.cancel(oldAbsintheSocket, notifier)

    const newNotifier = AbsintheSocket.send(absintheSocket, {
      operation: operation,
      variables: {}
    })

    AbsintheSocket.observe(absintheSocket, newNotifier, handlers)
  }

  resubscribe(newCommunitySubscription)
  resubscribe(transferSubscription)
  resubscribe(notificationSubscription)
  resubscribe(highlightedNewsSubscription)
})

async function handleJavascriptPort (arg) {
  switch (arg.data.name) {
    case 'checkAccountAvailability': {
      return eos.getAccount(arg.data.account)
        .then(_ => ({ isAvailable: false, error: 'account not available' }))
        .catch(e => {
          // Invalid name exception
          if (JSON.parse(e.message).error.code === 3010001) {
            return { isAvailable: false, error: e.message }
          } else {
            return { isAvailable: true }
          }
        })
    }
    case 'generateKeys': {
      const userLang = getUserLanguage()
      const [randomWords, hexRandomWords] = mnemonic.generateRandom(userLang)
      const privateKey = ecc.seedPrivate(hexRandomWords)
      const publicKey = ecc.privateToPublic(privateKey)

      return {
        data: {
          ownerKey: publicKey,
          activeKey: publicKey,
          accountName: arg.data.account,
          words: randomWords,
          privateKey: privateKey
        }
      }
    }
    case 'getAccountFrom12Words': {
      const { passphrase } = arg.data
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

      if (!ecc.isValidPrivate(privateKey)) {
        return { error: 'error.invalidKey' }
      } else {
        try {
          const publicKey = ecc.privateToPublic(privateKey)
          const accounts = await eos.getKeyAccounts(publicKey)

          if (!accounts || !accounts.account_names || accounts.account_names.length === 0) {
            return { error: 'error.accountNotFound' }
          } else {
            const [accountName] = accounts.account_names
            return { accountName, privateKey }
          }
        } catch (err) {
          logEvent({
            user: null,
            message: 'Get account from key port error',
            tags: { 'cambiatus.kind': 'auth' },
            contexts: [{ name: 'Error details', extras: { error: err } }],
            transaction: 'getAccountFromKey',
            level: 'error'
          })

          return { error: 'error.unknown' }
        }
      }
    }
    case 'signString': {
      const { input, privateKey } = arg.data

      const signed = ecc.sign(input, privateKey)

      return { signed }
    }
    case 'login': {
      const { privateKey, passphrase, accountName, pin } = arg.data

      logout()

      storePin({ accountName, passphrase, privateKey }, pin)

      // Save credentials to EOS
      eos = Eos(Object.assign(config.eosOptions, { keyProvider: privateKey }))
      if (env !== 'development') {
        Sentry.configureScope((scope) => { scope.setUser({ username: accountName }) })
      }

      addBreadcrumb({
        type: 'debug',
        category: 'login',
        message: 'Saved credentials to EOS',
        data: {},
        localData: { accountName },
        level: 'debug'
      })

      return {}
    }
    case 'changePin': {
      const userStorage = JSON.parse(getItem(USER_KEY))
      const currentPin = arg.data.currentPin
      const newPin = arg.data.newPin
      const decryptedKey = sjcl.decrypt(currentPin, userStorage.encryptedKey)

      const data = {
        accountName: userStorage.accountName,
        privateKey: decryptedKey
      }

      data.passphrase = sjcl.decrypt(
        currentPin,
        userStorage.encryptedPassphrase
      )

      storePin(data, newPin)

      return { accountName: arg.data.accountName, privateKey: decryptedKey }
    }
    case 'getPrivateKey': {
      const user = JSON.parse(getItem(USER_KEY))
      const pin = arg.data.pin
      // If private key and accountName are stored in localStorage
      const isUserLoggedIn = user && user.encryptedKey && user.accountName
      if (!isUserLoggedIn) {
        return { error: 'error.unavailablePin' }
      } else {
        try {
          const decryptedKey = sjcl.decrypt(pin, user.encryptedKey)

          eos = Eos(Object.assign(config.eosOptions, { keyProvider: decryptedKey }))
          addBreadcrumb({
            type: 'debug',
            category: 'getPrivateKey',
            message: 'Saved credentials to EOS',
            data: {},
            localData: {},
            level: 'debug'
          })

          // Configure Sentry logged user
          addBreadcrumb({
            type: 'info',
            category: 'getPrivateKey',
            message: 'Logged user with PIN',
            data: { accountName: user.accountName },
            localData: {},
            level: 'info'
          })

          return { accountName: user.accountName, privateKey: decryptedKey }
        } catch (e) {
          return { error: 'error.invalidPin' }
        }
      }
    }
    case 'eosTransaction': {
      addBreadcrumb({
        type: 'debug',
        category: 'eosTransaction',
        message: 'Started pushing transaction to EOS',
        data: {},
        localData: { actions: arg.data.actions },
        level: 'info'
      })

      return eos.transaction({ actions: arg.data.actions })
        .then(res => {
          addBreadcrumb({
            type: 'debug',
            category: 'eosTransaction',
            message: 'Finished pushing transaction to EOS without errors',
            data: { transactionId: res.transaction_id },
            localData: {},
            level: 'info'
          })

          return { transactionId: res.transaction_id }
        })
        .catch(errorString => {
          let error
          let errorMessage
          try {
            error = JSON.parse(errorString)
            try {
              errorMessage = `[EOS] ${error.error.details[0].message}`
            } catch {
              errorMessage = `[EOS] ${error.message}`
            }
          } catch {
            error = errorString
            errorMessage = 'Got an error when pushing transaction to EOS'
          }

          const response = { error }

          logEvent({
            user: null,
            message: errorMessage,
            tags: { 'cambiatus.type': 'eos-transaction' },
            contexts: [{
              name: 'Eos transaction',
              extras: {
                sent: arg.data,
                response,
                error
              }
            }],
            localData: {},
            transaction: 'eosTransaction',
            level: 'error'
          })

          return response
        })
    }
    case 'logout': {
      logout()

      return {}
    }
    case 'downloadAuthPdfFromRegistration': {
      const { accountName, passphrase } = arg.data
      return downloadPdf(accountName, passphrase)
    }
    case 'downloadAuthPdfFromProfile': {
      const store = JSON.parse(getItem(USER_KEY))
      const pin = arg.data.pin

      const decryptedPassphrase = sjcl.decrypt(pin, store.encryptedPassphrase)
      return downloadPdf(store.accountName, decryptedPassphrase)
    }
    case 'accountNameToUint64': {
      return { uint64name: eos.modules.format.encodeName(arg.data.accountName, false) }
    }
    case 'scrollIntoView': {
      // We might be creating the element and scrolling to it at the same time.
      // If we don't use setTimeout, we might try to scroll to the element before it's created, which produces a runtime error
      setTimeout(() => {
        document.getElementById(arg.data.id).scrollIntoView(true)
      }, 0)

      return {}
    }
    case 'smoothHorizontalScroll': {
      const { containerId, targetId } = arg.data

      const targetLeft = document.getElementById(targetId).getBoundingClientRect().left
      const container = document.getElementById(containerId)
      const offset = targetLeft - container.getBoundingClientRect().left
      container.scrollTo({
        left: container.scrollLeft + offset,
        behavior: 'smooth'
      })

      return {}
    }
    case 'subscribeToNewCommunity': {
      // Cancel existing notifier
      if (newCommunitySubscription && newCommunitySubscription.notifier) {
        AbsintheSocket.cancel(absintheSocket, newCommunitySubscription.notifier)
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {}
      })

      const onStart = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToNewCommunity',
          message: 'Started listening to new communities',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      const onAbort = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToNewCommunity',
          message: 'Aborted listening to new communities',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onCancel = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToNewCommunity',
          message: 'Cancelled listening to new communities',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onError = data => {
        addBreadcrumb({
          type: 'error',
          category: 'subscribeToNewCommunity',
          message: 'Error when listening to new communities',
          data: {},
          localData: { data },
          level: 'error'
        })
      }

      const onResult = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToNewCommunity',
          message: 'Got a new community result',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'responded'
        }
        app.ports.javascriptInPort.send(response)
      }

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult
      }

      newCommunitySubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription
      }

      AbsintheSocket.observe(absintheSocket, notifier, handlers)

      return { isSubscription: true }
    }
    case 'subscribeToTransfer': {
      // Cancel existing notifier
      if (transferSubscription && transferSubscription.notifier) {
        AbsintheSocket.cancel(absintheSocket, transferSubscription.notifier)
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {}
      })

      const onStart = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToTransfer',
          message: 'Started listening for transfer',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      const onAbort = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToTransfer',
          message: 'Aborted listening for transfer',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onCancel = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToTransfer',
          message: 'Cancelled listening for transfer',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onError = data => {
        addBreadcrumb({
          type: 'error',
          category: 'subscribeToTransfer',
          message: 'Error when listening for transfer',
          data: {},
          localData: { data },
          level: 'error'
        })
      }

      const onResult = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToTransfer',
          message: 'Got transfer result',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'responded',
          data: data
        }
        app.ports.javascriptInPort.send(response)
      }

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult
      }

      transferSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription
      }

      AbsintheSocket.observe(absintheSocket, notifier, handlers)

      return { isSubscription: true }
    }
    case 'subscribeToUnreadCount': {
      // Cancel existing notifier
      if (notificationSubscription && notificationSubscription.notifier) {
        AbsintheSocket.cancel(absintheSocket, notificationSubscription.notifier)
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {}
      })

      const onStart = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToUnreadCount',
          message: 'Started listening for unread notifications',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onAbort = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToUnreadCount',
          message: 'Aborted listening for unread notifications',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onCancel = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToUnreadCount',
          message: 'Cancelled listening for unread notifications',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onError = data => {
        addBreadcrumb({
          type: 'error',
          category: 'subscribeToUnreadCount',
          message: 'Error when listening for unread notifications',
          data: {},
          localData: { data },
          level: 'error'
        })
      }

      const onResult = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToUnreadCount',
          message: 'Got a result when listening for unread notifications',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          meta: data
        }
        app.ports.javascriptInPort.send(response)
      }

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult
      }

      notificationSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription
      }

      AbsintheSocket.observe(absintheSocket, notifier, handlers)

      return { isSubscription: true }
    }
    case 'subscribeToHighlightedNewsChanged': {
      // Cancel existing notifier
      if (highlightedNewsSubscription && highlightedNewsSubscription.notifier) {
        AbsintheSocket.cancel(absintheSocket, highlightedNewsSubscription.notifier)
      }

      // Create new notifier
      const notifier = AbsintheSocket.send(absintheSocket, {
        operation: arg.data.subscription,
        variables: {}
      })

      const onStart = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToHighlightedNewsChanged',
          message: 'Started listening for highlighted news changes',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onAbort = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToHighlightedNewsChanged',
          message: 'Aborted listening for highlighted news changes',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onCancel = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToHighlightedNewsChanged',
          message: 'Cancelled listening for highlighted news changes',
          data: {},
          localData: { data },
          level: 'info'
        })
      }

      const onError = data => {
        addBreadcrumb({
          type: 'error',
          category: 'subscribeToHighlightedNewsChanged',
          message: 'Error listening for highlighted news changes',
          data: {},
          localData: { data },
          level: 'error'
        })
      }

      const onResult = data => {
        addBreadcrumb({
          type: 'info',
          category: 'subscribeToHighlightedNewsChanged',
          message: 'Got a result when listening for highlighted news changes',
          data: {},
          localData: { data },
          level: 'info'
        })

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          meta: data
        }
        app.ports.javascriptInPort.send(response)
      }

      const handlers = {
        onAbort,
        onError,
        onCancel,
        onStart,
        onResult
      }

      highlightedNewsSubscription = {
        notifier,
        handlers,
        operation: arg.data.subscription
      }

      AbsintheSocket.observe(absintheSocket, notifier, handlers)

      return { isSubscription: true }
    }
    case 'copyToClipboard': {
      // We might need to want to change the dom before copying contents of the input
      await new Promise(function (resolve, reject) {
        window.setTimeout(async () => {
          const element = document.getElementById(arg.data.id)
          if (!element) {
            reject(new Error('Element not found'))
            return
          }

          // The clipboard API is not supported in all browsers
          if (navigator.clipboard && navigator.clipboard.writeText && element.value) {
            await navigator.clipboard.writeText(element.value)

            resolve()
            return
          }

          element.select()
          document.execCommand('copy')

          resolve()
        }, 0)
      })

      return { copied: true }
    }
    case 'readClipboard': {
      if (canReadClipboard()) {
        try {
          const permissionStatus = await navigator.permissions.query({
            name: 'clipboard-read',
            allowWithoutGesture: true
          })

          switch (permissionStatus.state) {
            case 'denied': {
              addBreadcrumb({
                type: 'info',
                category: 'readClipboard',
                message: 'Checked for clipboard access, and it\'s denied',
                data: {},
                localData: {},
                level: 'info'
              })

              // We can't request for permission, the user needs to do it manually
              return { isDenied: true }
            }
            case 'granted': {
              addBreadcrumb({
                type: 'info',
                category: 'readClipboard',
                message: 'Checked for clipboard access, and it\'s granted',
                data: {},
                localData: {},
                level: 'info'
              })

              return readClipboardWithPermission()
            }
            case 'prompt': {
              addBreadcrumb({
                type: 'debug',
                category: 'readClipboard',
                message: 'Prompting user for clipboard access',
                data: {},
                localData: {},
                level: 'debug'
              })

              try {
                const clipboardContent = await navigator.clipboard.readText()
                addBreadcrumb({
                  type: 'info',
                  category: 'readClipboard',
                  message: 'User granted access to clipboard',
                  data: {},
                  localData: { clipboardContent },
                  level: 'info'
                })

                return { clipboardContent }
              } catch (err) {
                addBreadcrumb({
                  type: 'info',
                  category: 'readClipboard',
                  message: 'User denied access to clipboard',
                  data: {},
                  localData: { error: err },
                  level: 'info'
                })

                return { isDenied: true }
              }
            }
            default: {
              return { error: 'permissionStatus state unkown' }
            }
          }
        } catch (permissionError) {
          addBreadcrumb({
            type: 'info',
            category: 'readClipboard',
            message: 'The permissions API is not supported by the user\'s browser',
            data: { permissionError },
            localData: {},
            level: 'info'
          })

          return readClipboardWithPermission()
        }
      } else {
        addBreadcrumb({
          type: 'info',
          category: 'readClipboard',
          message: 'clipboard.readText() is not supported by the user\'s browser',
          data: {},
          localData: {},
          level: 'info'
        })

        return { notSupported: true }
      }
    }
    case 'share': {
      const { title, text, url } = arg.data

      try {
        await navigator.share({ title, text, url })
        return {}
      } catch (err) {
        return { error: err }
      }
    }
    case 'setFavicon': {
      const { favicon } = arg.data
      document.head.querySelectorAll('link[rel*=icon]')
        .forEach((icon) => {
          icon.href = favicon
        })

      return {}
    }
    case 'addClassToDocument': {
      const { className } = arg.data

      document.querySelector('html').classList.add(className)
      document.querySelector('body').classList.add(className)

      return {}
    }
    case 'removeClassFromDocument': {
      const { className } = arg.data

      document.querySelector('html').classList.remove(className)
      document.querySelector('body').classList.remove(className)

      return {}
    }
    case 'getBip39': {
      const normalize = (wordlist) => {
        return wordlist.map(word => word.normalize())
      }

      return {
        english: normalize(bip39.wordlists.english),
        portuguese: normalize(bip39.wordlists.portuguese),
        spanish: normalize(bip39.wordlists.spanish)
      }
    }
    case 'addMatomoScript': {
      matomo.addScript()

      return {}
    }
    default: {
      return { error: `No treatment found for Elm port ${arg.data.name}` }
    }
  }
}
