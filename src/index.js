import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import sjcl from 'sjcl'
import './styles/main.css'
import mnemonic from './scripts/mnemonic.js'
import configuration from './scripts/config.js'
// import registerServiceWorker from './scripts/registerServiceWorker'
import pdfDefinition from './scripts/pdfDefinition'
import * as pushSub from './scripts/pushNotifications'
import { Elm } from './elm/Main.elm'
import * as Sentry from '@sentry/browser'
import * as AbsintheSocket from '@absinthe/socket'
import pdfMake from 'pdfmake/build/pdfmake'
import pdfFonts from './vfs_fonts'

// =========================================
// App startup
// =========================================

let eos = null
const USER_KEY = 'bespiral.user'
const LANGUAGE_KEY = 'bespiral.language'
const PUSH_PREF = 'bespiral.push.pref'
const AUTH_TOKEN = 'bespiral.auth_token'
const RECENT_SEARCHES = 'bespiral.recent_search'
const env = process.env.NODE_ENV || 'development'
const graphqlSecret = process.env.GRAPHQL_SECRET || ''
const useSubdomain = process.env.USE_SUBDOMAIN
const config = configuration[env]

const GLOBAL_STORAGE_IFRAME_ID = 'cambiatus-globalstorage-iframe'

const main = (setupIframe) => {
  const operationsBeforeIframeLoad = []

  let isIframeLoaded = false

  const getItem = (key) => window.localStorage.getItem(key)

  const removeItem = (key) => {
    const iframe = document.getElementById(GLOBAL_STORAGE_IFRAME_ID)
    if (isIframeLoaded && iframe) {
      iframe.contentWindow.postMessage({
        method: 'remove',
        key
      }, config.endpoints.globalStorage)
    } else {
      operationsBeforeIframeLoad.push({ method: 'remove', key })
    }
    window.localStorage.removeItem(key)
  }

  const setItem = (key, value) => {
    const iframe = document.getElementById(GLOBAL_STORAGE_IFRAME_ID)
    if (isIframeLoaded && iframe) {
      iframe.contentWindow.postMessage({
        method: 'set',
        key,
        value
      }, config.endpoints.globalStorage)
    } else {
      operationsBeforeIframeLoad.push({ method: 'set', key, value })
    }
    window.localStorage.setItem(key, value)
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
    window.mnemonic = mnemonic
    window.ecc = ecc
    window.bip39 = require('bip39')
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
          hasBody: function (obj) {},
          body: function (obj, config) {}
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
    return !!navigator.clipboard && !!navigator.clipboard.readText
  }

  /** Assumes we already have clipboard permissions */
  async function readClipboardWithPermission () {
    try {
      const clipboardContent = await navigator.clipboard.readText()
      return { clipboardContent }
    } catch (err) {
      errorLog('clipboard.readText() error', err, false)
      return { error: err.message }
    }
  }

  function flags () {
    const user = JSON.parse(getItem(USER_KEY))
    return {
      env: env,
      graphqlSecret: graphqlSecret,
      endpoints: config.endpoints,
      language: getUserLanguage(),
      accountName: (user && user.accountName) || null,
      isPinAvailable: !!(user && user.encryptedKey),
      authToken: getItem(AUTH_TOKEN),
      logo: config.logo,
      logoMobile: config.logoMobile,
      now: Date.now(),
      allowCommunityCreation: config.allowCommunityCreation,
      tokenContract: config.tokenContract,
      communityContract: config.communityContract,
      canReadClipboard: canReadClipboard(),
      useSubdomain: useSubdomain
    }
  }

  // Start elm app with flags
  const app = Elm.Main.init({
    flags: flags()
  })
  setupIframe(false, (iframe) => {
    operationsBeforeIframeLoad.forEach(operation => {
      iframe.contentWindow.postMessage(operation, config.endpoints.globalStorage)
    })

    isIframeLoaded = true
  })

  Sentry.addBreadcrumb({
    message: 'Started Elm app',
    level: Sentry.Severity.Info,
    type: 'debug',
    category: 'started',
    data: {
      flags: flags()
    }
  })

  // Register Service Worker After App
  // registerServiceWorker()

  // Log
  function debugLog (name, arg) {
    if (env === 'development') {
      console.log('[==== DEV]: ', name, arg)
    } else {
      Sentry.addBreadcrumb({ message: name, level: 'info', type: 'debug' })
    }
  }

  // Init Sentry
  Sentry.init({
    dsn: 'https://535b151f7b8c48f8a7307b9bc83ebeba@sentry.io/1480468',
    environment: env
  })

  function errorLog (msg, err, isFromElm) {
    const languageString = isFromElm ? 'Elm' : 'Javascript'
    const startMessage = `Begin errorLog from ${languageString}`
    Sentry.addBreadcrumb({
      message: startMessage,
      level: Sentry.Severity.Info,
      type: 'debug',
      category: 'started'
    })

    if (env === 'development') {
      console.error(msg, err)
    } else {
      let error = `Generic ${languageString} errorLog`
      let details = ''

      if (Object.prototype.toString.call(msg) === '[object Array]') {
        [error, details] = msg
      }

      const type = isFromElm ? 'elm-error' : 'javascript-error'
      Sentry.withScope(scope => {
        scope.setTag('type', type)
        scope.setLevel(Sentry.Severity.Error)
        scope.setExtra(`Error shared by ${languageString}`, err)
        scope.setExtra('raw msg', msg)
        scope.setExtra('Parsed details', details)
        Sentry.captureMessage(error + details)
      })
    }

    const endMessage = `End errorLog from ${languageString}`
    Sentry.addBreadcrumb({
      message: endMessage,
      level: Sentry.Severity.Info,
      type: 'debug',
      category: 'ended'
    })
  }

  // Ports error Reporter
  app.ports.logError.subscribe((msg, err) => { errorLog(msg, err, true) })

  app.ports.logDebug.subscribe(debugLog)

  // =========================================
  // EOS / Identity functions
  // =========================================

  eos = Eos(config.eosOptions)

  // STORE LANGUAGE

  app.ports.storeLanguage.subscribe(storeLanguage)

  function storeLanguage (lang) {
    setItem(LANGUAGE_KEY, lang)
    debugLog(`stored language: ${lang}`, '')
  }

  // STORE RECENT SEARCHES
  app.ports.storeRecentSearches.subscribe(query => {
    setItem(RECENT_SEARCHES, query)
    debugLog(`stored recent searches: ${query}`, '')
  })

  // RETRIEVE RECENT SEARCHES
  app.ports.getRecentSearches.subscribe(() => {
    const recentSearches = getItem(RECENT_SEARCHES) || '[]'
    app.ports.gotRecentSearches.send(recentSearches)
    debugLog(`got recent searches: ${recentSearches}`, '')
  })

  app.ports.storeAuthToken.subscribe(token => {
    setItem(AUTH_TOKEN, token)
    debugLog(`stored auth token`, token)
  })

  // STORE PUSH PREF

  function storePushPref (pref) {
    setItem(PUSH_PREF, pref)
    debugLog(`stored push pref: ${pref}`, '')
  }

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
    debugLog('stored pin', pin)
  }

  function logout () {
    removeItem(USER_KEY)
    removeItem(AUTH_TOKEN)

    Sentry.addBreadcrumb({
      category: 'auth',
      message: 'User logged out'
    })
    Sentry.setUser(null)
    debugLog('set sentry user to null', '')
  }

  function downloadPdf (accountName, passphrase) {
    debugLog('started downloadPdf', accountName)
    const definition = pdfDefinition(passphrase)
    const pdf = pdfMake.createPdf(definition)

    pdf.download(accountName + '_cambiatus.pdf')

    debugLog('downloadPdf finished', '')

    return { isDownloaded: true }
  }

  app.ports.javascriptOutPort.subscribe(async (arg) => {
    debugLog(`${arg.data.name} port started`, arg)
    const defaultResponse = {
      address: arg.responseAddress,
      addressData: arg.responseData
    }

    const portResponse = await handleJavascriptPort(arg)
    debugLog(`got ${arg.data.name} port response`, portResponse)

    const response = { ...defaultResponse, ...portResponse }

    if (response.error) {
      debugLog(`${arg.data.name} port failed: ${response.error}`, response)
    } else {
      debugLog(`${arg.data.name} port finished`, response)
    }

    // Only send data through port if there is a portResponse, and the port is not a subscription
    if (Object.keys(portResponse).length !== 0 && !response.isSubscription) {
      app.ports.javascriptInPort.send(response)
    }
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
      case 'login': {
        const passphrase = arg.data.passphrase
        const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

        if (!ecc.isValidPrivate(privateKey)) {
          return { error: 'error.invalidKey' }
        } else {
          try {
            const publicKey = ecc.privateToPublic(privateKey)
            const accounts = await eos.getKeyAccounts(publicKey)
            debugLog(`got ${accounts.account_names.length} accounts`, accounts)

            if (!accounts || !accounts.account_names || accounts.account_names.length === 0) {
              return { error: 'error.accountNotFound' }
            } else {
              const accountName = accounts.account_names[0]

              logout()

              storePin(
                {
                  accountName,
                  privateKey,
                  passphrase
                },
                arg.data.pin
              )

              // Save credentials to EOS
              eos = Eos(Object.assign(config.eosOptions, { keyProvider: privateKey }))
              debugLog('saved credentials to EOS', '')

              // Configure Sentry logged user
              Sentry.setUser({ email: accountName })
              debugLog('set sentry user', accountName)

              return { accountName, privateKey }
            }
          } catch (err) {
            errorLog('login port error', err, false)
            return { error: 'error.unknown' }
          }
        }
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

        // `.encryptedPassphrase` property was added in https://github.com/cambiatus/frontend/pull/270 while redesigning
        // the Profile page. For the users who were already logged-in before these changes were introduced,
        // this property may not exist.
        if (userStorage.encryptedPassphrase) {
          data.passphrase = sjcl.decrypt(
            currentPin,
            userStorage.encryptedPassphrase
          )
        }

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
            debugLog('saved credentials to EOS', '')

            // Configure Sentry logged user
            Sentry.setUser({ account: user.accountName })
            debugLog('set sentry user', user.accountName)

            Sentry.addBreadcrumb({
              category: 'auth',
              level: Sentry.Severity.Info,
              message: 'Logged user with PIN: ' + user.accountName
            })

            return { accountName: user.accountName, privateKey: decryptedKey }
          } catch (e) {
            return { error: 'error.invalidPin' }
          }
        }
      }
      case 'eosTransaction': {
        Sentry.addBreadcrumb({
          type: 'debug',
          category: 'started',
          level: 'info',
          message: 'Begin pushing transaction to EOS'
        })

        return eos.transaction({ actions: arg.data.actions })
          .then(res => {
            Sentry.addBreadcrumb({
              type: 'debug',
              category: 'ended',
              level: 'info',
              message: 'Success pushing transaction to EOS'
            })

            return { transactionId: res.transaction_id }
          })
          .catch(errorString => {
            let error
            try {
              error = JSON.parse(errorString)
            } catch {
              error = errorString
            }

            const response = { error }

            // Send to sentry
            Sentry.addBreadcrumb({
              type: 'default',
              category: 'sentry.transaction',
              level: 'info',
              message: 'Failure pushing transaction to EOS'
            })
            Sentry.withScope(scope => {
              let message
              try {
                message = error.error.details[0].message || 'Generic EOS Error'
              } catch {
                message = errorString
              }

              scope.setTag('type', 'eos-transaction')
              scope.setExtra('Sent data', arg.data)
              scope.setExtra('Response', response)
              scope.setExtra('Error', response.error)
              scope.setExtra('Error String', errorString)
              scope.setLevel(Sentry.Severity.Error)
              Sentry.captureMessage(message)
            })

            return response
          })
      }
      case 'logout': {
        logout()

        return {}
      }
      case 'requestPushPermission': {
        const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`
        const pKey = config.pushKey
        if (pushSub.isPushSupported()) {
          return navigator.serviceWorker
            .register(swUrl)
            .then(sw => pushSub.askPermission())
            .then(sw => pushSub.subscribeUserToPush(pKey))
            .then(sub => {
              return { sub: JSON.stringify(sub) }
            })
            .catch(err => {
              debugLog('requestPushPermission port error: Push Permission Denied', err)
              return { error: `push permission denied: ${err}` }
            })
        } else {
          debugLog('requestPushPermission port error: Push not supported on this agent', '')
          return { error: 'push not supported on this agent' }
        }
      }
      case 'completedPushUpload': {
        storePushPref('set')
        return { isSet: true }
      }
      case 'checkPushPref': {
        return { isSet: getItem(PUSH_PREF) !== null }
      }
      case 'disablePushPref': {
        removeItem(PUSH_PREF)
        pushSub.unsubscribeFromPush()
        return { isSet: false }
      }
      case 'downloadAuthPdfFromRegistration': {
        const { accountName, passphrase } = arg.data
        return downloadPdf(accountName, passphrase)
      }
      case 'downloadAuthPdfFromProfile': {
        const store = JSON.parse(getItem(USER_KEY))
        const pin = arg.data.pin

        // `.encryptedPassphrase` property was added in https://github.com/cambiatus/frontend/pull/270 while redesigning
        // the Profile page. For the users who were already logged-in before these changes were introduced,
        // this property may not exist. This case is handled by passing `isDownloaded: false` to Elm
        // for further processing.
        if (store.encryptedPassphrase) {
          const decryptedPassphrase = sjcl.decrypt(pin, store.encryptedPassphrase)
          return downloadPdf(store.accountName, decryptedPassphrase)
        } else {
          // The case when there's not passphrase stored in user's browser, only the Private Key
          return { isDownloaded: false }
        }
      }
      case 'accountNameToUint64': {
        return { uint64name: eos.modules.format.encodeName(arg.data.accountName, false) }
      }
      case 'scrollIntoView': {
        document.getElementById(arg.data.id).scrollIntoView(true)

        return {}
      }
      case 'validateDeadline': {
        const parsedDate = new Date(arg.data.deadline)
        const now = new Date()

        if (parsedDate.toString() === 'Invalid Date' || parsedDate < now) {
          return { error: parsedDate }
        } else {
          const isoDate = parsedDate.toISOString()
          return { date: isoDate }
        }
      }
      case 'hideFooter': {
        document.getElementById('guest-footer').className += ' guest__footer'
        return {}
      }
      case 'subscribeToNewCommunity': {
        let notifiers = []

        // Open a socket connection
        const socketConn = new PhoenixSocket(config.endpoints.socket)

        // Build a graphql Socket
        const abSocket = AbsintheSocket.create(socketConn)

        // Remove existing notifiers if any
        notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

        // Create new notifiers
        notifiers = [arg.data.subscription].map(operation =>
          AbsintheSocket.send(abSocket, {
            operation,
            variables: {}
          })
        )

        const onStart = data => {
          debugLog('subscribeToNewCommunity port: onStart handler called', data)
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            state: 'starting'
          }
          app.ports.javascriptInPort.send(response)
        }

        const onAbort = data => {
          debugLog('subscribeToNewCommunity port: onAbort handler called', data)
        }

        const onCancel = data => {
          debugLog('subscribeToNewCommunity port: onCancel handler called', data)
        }

        const onError = data => {
          debugLog('subscribeToNewCommunity port: onError handler called', data)
        }

        let onResult = data => {
          debugLog('subscribeToNewCommunity port: onResult handler called', data)
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            state: 'responded'
          }
          app.ports.javascriptInPort.send(response)
        }

        notifiers.map(notifier => {
          AbsintheSocket.observe(abSocket, notifier, {
            onAbort,
            onError,
            onCancel,
            onStart,
            onResult
          })
        })

        return { isSubscription: true }
      }
      case 'subscribeToTransfer': {
        let notifiers = []

        // Open a socket connection
        const socketConn = new PhoenixSocket(config.endpoints.socket)

        // Build a graphql Socket
        const abSocket = AbsintheSocket.create(socketConn)

        // Remove existing notifiers if any
        notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

        // Create new notifiers
        notifiers = [arg.data.subscription].map(operation =>
          AbsintheSocket.send(abSocket, {
            operation,
            variables: {}
          })
        )

        let onStart = data => {
          debugLog('subscribeToTransfer port: onStart handler called', data)

          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            state: 'starting'
          }
          app.ports.javascriptInPort.send(response)
        }

        const onAbort = data => {
          debugLog('subscribeToTransfer port: onAbort handler called', data)
        }

        const onCancel = data => {
          debugLog('subscribeToTransfer port: onCancel handler called', data)
        }

        const onError = data => {
          debugLog('subscribeToTransfer port: onError handler called', data)
        }

        const onResult = data => {
          debugLog('subscribeToTransfer port: onResult handler called', data)
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            state: 'responded',
            data: data
          }
          app.ports.javascriptInPort.send(response)
        }

        notifiers.map(notifier => {
          AbsintheSocket.observe(abSocket, notifier, {
            onAbort,
            onError,
            onCancel,
            onStart,
            onResult
          })
        })

        return { isSubscription: true }
      }
      case 'subscribeToUnreadCount': {
        let notifiers = []

        // Open a socket connection
        const socketConn = new PhoenixSocket(config.endpoints.socket)

        // Build a graphql Socket
        const abSocket = AbsintheSocket.create(socketConn)

        // Remove existing notifiers if any
        notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

        // Create new notifiers
        notifiers = [arg.data.subscription].map(operation =>
          AbsintheSocket.send(abSocket, {
            operation,
            variables: {}
          })
        )

        const onStart = data => {
          const payload = { dta: data, msg: 'starting unread countsubscription' }
          debugLog('subscribeToUnreadCount port: onStart handler called', payload)
        }

        const onAbort = data => {
          debugLog('subscribeToUnreadCount port: onAbort handler called', data)
        }

        const onCancel = data => {
          debugLog('subscribeToUnreadCount port: onCancel handler called', data)
        }

        const onError = data => {
          debugLog('subscribeToUnreadCount port: onError handler called', data)
        }

        const onResult = data => {
          debugLog('subscribeToUnreadCount port: onResult handler called', data)
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            meta: data
          }
          app.ports.javascriptInPort.send(response)
        }

        notifiers.map(notifier => {
          AbsintheSocket.observe(abSocket, notifier, {
            onAbort,
            onError,
            onCancel,
            onStart,
            onResult
          })
        })

        return { isSubscription: true }
      }
      case 'copyToClipboard': {
        document.querySelector('#' + arg.data.id).select()
        document.execCommand('copy')

        return {}
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
                debugLog('clipboard-read permission denied', '')
                // We can't request for permission, the user needs to do it manually
                return { isDenied: true }
              }
              case 'granted': {
                debugLog('clipboard-read permission granted', '')
                return readClipboardWithPermission()
              }
              case 'prompt': {
                debugLog('clipboard-read permission prompt', '')
                try {
                  const clipboardContent = await navigator.clipboard.readText()
                  debugLog('clipboard-read permission prompt accepted', '')
                  return { clipboardContent }
                } catch (err) {
                  debugLog('clipboard-read permission prompt denied', err)
                  return { isDenied: true }
                }
              }
              default: {
                return { error: 'permissionStatus state unkown' }
              }
            }
          } catch (permissionError) {
            debugLog('permissions API not supported', permissionError)
            return readClipboardWithPermission()
          }
        } else {
          debugLog('clipboard.readText() not supported', '')
          return { notSupported: true }
        }
      }
      default: {
        return { error: `No treatment found for Elm port ${arg.data.name}` }
      }
    }
  }
}

const mainApp = () => {
  const setupIframe = (isInitial, onLoad) => {
    const iframe = document.createElement('iframe')
    const src = config.endpoints.globalStorage

    iframe.onload = () => {
      const contentWindow = iframe.contentWindow
      const postMessage = (message) => {
        contentWindow.postMessage(message, src)
      }

      if (isInitial) {
        postMessage({
          method: 'getMany',
          keys: [USER_KEY, LANGUAGE_KEY, PUSH_PREF, AUTH_TOKEN, RECENT_SEARCHES]
        })
      }

      onLoad(iframe)
    }

    iframe.src = src
    iframe.style = 'display: none'
    iframe.id = GLOBAL_STORAGE_IFRAME_ID
    document.body.appendChild(iframe)
  }

  let hasRunMain = false
  // Receive a `getMany` message, and write all of the content to localStorage
  window.onmessage = (e) => {
    const payload = e.data

    if (payload.method && payload.method === 'getMany') {
      payload.data.forEach(({ key, value }) => {
        if (value) {
          window.localStorage.setItem(key, value)
        } else {
          window.localStorage.removeItem(key)
        }
      })

      if (!hasRunMain) {
        hasRunMain = true
        main(setupIframe)
      }
    }
  }

  // Read globalStorage data on load
  window.onload = () => {
    setupIframe(true, (iframe) => {})
  }
}

const globalStorage = () => {
  const allowedDomains = ['localhost', 'cambiatus.io']

  window.onmessage = (e) => {
    const payload = e.data
    const isAllowed = allowedDomains.some(
      (domain) =>
        e.origin.endsWith(domain) || e.origin.includes(`${domain}/`) || e.origin.includes(`${domain}:`)
    )

    if (!isAllowed || !payload.method) {
      return
    }

    const respond = (message) => {
      window.parent.postMessage(message, e.origin)
    }

    switch (payload.method) {
      case 'set':
        window.localStorage.setItem(payload.key, payload.value)
        break
      case 'get':
        respond({ data: window.localStorage.getItem(payload.key) })
        break
      case 'getMany':
        respond({
          method: 'getMany',
          data: payload.keys.map((key) => ({
            key, value: window.localStorage.getItem(key)
          }))
        })
        break
      case 'remove':
        window.localStorage.removeItem(payload.key)
        break
      default:
        break
    }
  }
}

if (window.location.pathname === '/globalstorage') {
  globalStorage()
} else {
  mainApp()
}
