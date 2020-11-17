import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import sjcl from 'sjcl'
import './styles/main.css'
import mnemonic from './scripts/mnemonic.js'
import configuration from './scripts/config.js'
import registerServiceWorker from './scripts/registerServiceWorker'
import pdfDefinition from './scripts/pdfDefinition'
import * as pushSub from './scripts/pushNotifications'
import { Elm } from './elm/Main.elm'
import * as Sentry from '@sentry/browser'
import * as AbsintheSocket from '@absinthe/socket'
import pdfMake from 'pdfmake/build/pdfmake'
import pdfFonts from './vfs_fonts'
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

// =========================================
// App startup
// =========================================

let eos = null
const USER_KEY = 'bespiral.user'
const LANGUAGE_KEY = 'bespiral.language'
const AUTH_PREF_KEY = 'bespiral.auth.pref'
const PUSH_PREF = 'bespiral.push.pref'
const SELECTED_COMMUNITY_KEY = 'bespiral.selected_community'
const env = process.env.NODE_ENV || 'development'
const config = configuration[env]

function getUserLanguage () {
  const urlParams = new URLSearchParams(window.location.search)

  return (
    urlParams.get('lang') ||
    window.localStorage.getItem(LANGUAGE_KEY) ||
    navigator.language ||
    navigator.userLanguage ||
    'en-US'
  )
}

function flags () {
  const user = JSON.parse(window.localStorage.getItem(USER_KEY))
  var flags_ = {
    env: env,
    endpoints: config.endpoints,
    language: getUserLanguage(),
    accountName: (user && user.accountName) || null,
    isPinAvailable: !!(user && user.encryptedKey),
    authPreference: window.localStorage.getItem(AUTH_PREF_KEY),
    logo: config.logo,
    logoMobile: config.logoMobile,
    now: Date.now(),
    allowCommunityCreation: config.allowCommunityCreation,
    selectedCommunity: getSelectedCommunity() || config.selectedCommunity,
    tokenContract: config.tokenContract,
    communityContract: config.communityContract
  }
  debugLog('flags', flags_)
  return flags_
}

// Start elm app with flags
const app = Elm.Main.init({
  flags: flags()
})
Sentry.addBreadcrumb({
  message: 'Started Elm app',
  level: 'info',
  type: 'debug',
  category: 'started',
  data: {
    flags: flags()
  }
})

// Register Service Worker After App
registerServiceWorker()

// Log

function debugLog (name, arg) {
  if (env === 'development') {
    console.log('[dev]', name, arg)
  }

  Sentry.addBreadcrumb({
    message: typeof arg === 'string' || arg instanceof String ? arg : name,
    level: 'info',
    type: 'debug'
  }
  )
}

// Init Sentry
Sentry.init({
  dsn: 'https://535b151f7b8c48f8a7307b9bc83ebeba@sentry.io/1480468',
  environment: env
})

// Ports error Reporter
app.ports.logError.subscribe((msg, err) => {
  if (env === 'development') {
    console.error(msg, err)
  } else {
    Sentry.withScope(scope => {
      scope.setExtra(msg)
      scope.setTag('type', 'port-error')
      scope.setTag('event', msg)
      scope.setLevel(Sentry.Severity.Error)
      Sentry.captureException(err)
    })
  }
})

app.ports.logDebug.subscribe(debugLog)

// =========================================
// EOS / Identity functions
// =========================================

eos = Eos(config.eosOptions)

// STORE LANGUAGE

app.ports.storeLanguage.subscribe(storeLanguage)

function storeLanguage (lang) {
  window.localStorage.setItem(LANGUAGE_KEY, lang)
}

// STORE AUTH PREFERENCE

function storeAuthPreference (auth) {
  window.localStorage.setItem(AUTH_PREF_KEY, auth)
}

// STORE ACCOUNTNAME

function storeAccountName (accountName) {
  var storeData = {
    accountName: accountName
  }
  window.localStorage.removeItem(USER_KEY)
  window.localStorage.setItem(USER_KEY, JSON.stringify(storeData))
}

// STORE PUSH PREF

function storePushPref (pref) {
  window.localStorage.setItem(PUSH_PREF, pref)
}

// STORE PIN

async function storePin (data, pin) {
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

  window.localStorage.removeItem(USER_KEY)
  window.localStorage.setItem(USER_KEY, JSON.stringify(storeData))
}

function getSelectedCommunity () {
  return window.localStorage.getItem(SELECTED_COMMUNITY_KEY)
}

function downloadPdf (accountName, passphrase, responseAddress, responseData) {
  const definition = pdfDefinition(passphrase)
  const pdf = pdfMake.createPdf(definition)

  pdf.download(accountName + '_cambiatus.pdf')

  Sentry.addBreadcrumb(
    {
      type: 'debug',
      message: 'downloaded PDF'
    }
  )
  const response = {
    address: responseAddress,
    addressData: responseData,
    isDownloaded: true
  }

  app.ports.javascriptInPort.send(response)
}

app.ports.javascriptOutPort.subscribe(handleJavascriptPort)
async function handleJavascriptPort (arg) {
  debugLog('handleJavascriptPort', arg)
  switch (arg.data.name) {
    case 'checkAccountAvailability': {
      debugLog('=========================', 'checkAccountAvailability')
      var sendResponse = function (isAvailable, error) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isAvailable: isAvailable,
          error: error
        }
        debugLog('checkAccountAvailability response', response)
        app.ports.javascriptInPort.send(response)
      }
      eos
        .getAccount(arg.data.account)
        .then(_ => sendResponse(false))
        .catch(e => {
          // Invalid name exception
          debugLog('checkAccountAvailability', e)
          if (JSON.parse(e.message).error.code === 3010001) {
            sendResponse(false)
          } else {
            sendResponse(true)
          }
        })
      break
    }
    case 'generateKeys': {
      debugLog('=========================', 'generateKeys')
      const userLang = getUserLanguage()
      const [randomWords, hexRandomWords] = mnemonic.generateRandom(userLang)
      const privateKey = ecc.seedPrivate(hexRandomWords)
      const publicKey = ecc.privateToPublic(privateKey)

      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        data: {
          ownerKey: publicKey,
          activeKey: publicKey,
          accountName: arg.data.account,
          words: randomWords,
          privateKey: privateKey
        }
      }

      debugLog('generateKeys response', response)
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'loginWithPrivateKey': {
      debugLog('=========================', 'loginWithPrivateKey')
      const passphrase = arg.data.form.passphrase
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

      if (ecc.isValidPrivate(privateKey)) {
        const publicKey = ecc.privateToPublic(privateKey)
        const accounts = await eos.getKeyAccounts(publicKey)
        const user = JSON.parse(window.localStorage.getItem(USER_KEY))
        debugLog('loginWithPrivateKey:accounts', accounts)

        if (
          !accounts ||
          !accounts.account_names ||
          accounts.account_names.length === 0
        ) {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: 'error.accountNotFound'
          }
          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        } else if (
          user &&
          user.accountName &&
          !accounts.account_names.some(aN => aN === user.accountName)
        ) {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error:
              'This private key does not correspond to logged in account. Please logout to use a different account.'
          }
          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        } else if (accounts.account_names.length === 1) {
          arg.data.name = 'loginWithPrivateKeyAccount'
          arg.data.accountName = accounts.account_names[0]
          handleJavascriptPort(arg)
        } else if (
          user &&
          user.accountName &&
          accounts.account_names.some(aN => aN === user.accountName)
        ) {
          arg.data.name = 'loginWithPrivateKeyAccount'
          arg.data.accountName = user.accountName
          handleJavascriptPort(arg)

          // There is multiple accounts
        } else {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            accountNames: accounts.account_names
          }
          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        }
      } else {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          error: 'Invalid key'
        }
        debugLog('response', response)
        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'loginWithPrivateKeyAccount': {
      debugLog('========================', 'loginWithPrivateKeyAccount')
      const loginForm = arg.data.form
      const accountName = arg.data.accountName
      const passphrase = loginForm.passphrase
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

      // Storing stuff
      if (loginForm.usePin) {
        storePin(
          {
            accountName: accountName,
            privateKey: privateKey,
            passphrase: passphrase
          },
          loginForm.usePin
        )
        storeAuthPreference('pin')
      } else {
        storeAccountName(accountName)
        storeAuthPreference('private_key')
      }

      // Save credentials to EOS
      eos = Eos(Object.assign(config.eosOptions, { keyProvider: privateKey }))

      // Configure Sentry logged user
      Sentry.setUser({ email: accountName })

      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        accountName: accountName,
        privateKey: privateKey
      }
      debugLog('response', response)
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'changePin': {
      debugLog('========================', 'changePin')

      const userStorage = JSON.parse(window.localStorage.getItem(USER_KEY))
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

      await storePin(data, newPin)

      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        accountName: arg.data.accountName,
        privateKey: decryptedKey
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'loginWithPin': {
      debugLog('========================', 'loginWithPin')
      const store = JSON.parse(window.localStorage.getItem(USER_KEY))
      const pin = arg.data.pin
      if (store && store.encryptedKey && store.accountName) {
        try {
          const decryptedKey = sjcl.decrypt(pin, store.encryptedKey)

          eos = Eos(Object.assign(config.eosOptions, { keyProvider: decryptedKey }))

          // Configure Sentry logged user
          Sentry.setUser({ account: store.accountName })

          storeAuthPreference('pin')

          // Set default selected community
          window.localStorage.setItem(
            SELECTED_COMMUNITY_KEY,
            flags().selectedCommunity
          )

          Sentry.addBreadcrumb({
            category: 'auth',
            level: Sentry.Severity.Info,
            message: 'Logged user with PIN: ' + store.accountName
          })

          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            accountName: store.accountName,
            privateKey: decryptedKey
          }

          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        } catch (e) {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: 'Invalid PIN'
          }
          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        }
      } else {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          error: 'PIN is unavailable'
        }
        debugLog('response', response)
        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'eosTransaction': {
      debugLog('=========================', 'transaction')
      debugLog('DATA', arg.data)

      Sentry.addBreadcrumb({
        type: 'debug',
        category: 'started',
        level: 'info',
        message: 'Begin pushing transaction to EOS'
      })

      eos
        .transaction({
          actions: arg.data.actions
        })
        .then(res => {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            transactionId: res.transaction_id
          }
          Sentry.addBreadcrumb({
            type: 'debug',
            category: 'ended',
            level: 'info',
            message: 'Success pushing transaction to EOS'
          })

          debugLog('eos.transaction.succeed', res)
          debugLog('response', response)
          app.ports.javascriptInPort.send(response)
        })
        .catch(errorString => {
          const error = JSON.parse(errorString)
          const errorResponse = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: error
          }
          debugLog('eos.transaction.failed', errorResponse)

          // Send to sentry
          Sentry.addBreadcrumb({
            type: 'default',
            category: 'sentry.transaction',
            level: 'info',
            message: 'Failure pushing transaction to EOS'
          })
          Sentry.configureScope(scope => {
            const message = error.error.details[0].message || 'Generic EOS Error'
            scope.setTag('type', 'eos-transaction')
            scope.setExtra('Sent data', arg.data)
            scope.setExtra('Response', errorResponse)
            scope.setExtra('Error', errorResponse.error)
            scope.setExtra('Error String', errorString)
            scope.setLevel(Sentry.Severity.Error)
            Sentry.captureMessage(message)
          })
          app.ports.javascriptInPort.send(errorResponse)
        })
      break
    }
    case 'logout': {
      debugLog('=========================', 'logout')
      window.localStorage.removeItem(USER_KEY)
      window.localStorage.removeItem(AUTH_PREF_KEY)
      window.localStorage.removeItem(SELECTED_COMMUNITY_KEY)
      Sentry.addBreadcrumb({
        category: 'auth',
        message: 'User logged out'
      })
      Sentry.setUser(null)
      break
    }
    case 'requestPushPermission': {
      debugLog('======================', 'requestingPushPermissions')
      const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`
      const pKey = config.pushKey
      if (pushSub.isPushSupported()) {
        return navigator.serviceWorker
          .register(swUrl)
          .then(sw => pushSub.askPermission())
          .then(sw => pushSub.subscribeUserToPush(pKey))
          .then(sub => {
            const stringSub = JSON.stringify(sub)
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              sub: stringSub
            }
            debugLog('requestPushPermission response', response)
            app.ports.javascriptInPort.send(response)
          })
          .catch(err => debugLog('Push Permission Denied', err))
      } else {
        debugLog('=======================', 'Push not supported on this agent')
      }
      break
    }
    case 'completedPushUpload': {
      debugLog('=====================', 'cachingPushSubscription')
      storePushPref('set')
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        isSet: true
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'checkPushPref': {
      debugLog('=====================', 'checkingPushPref')
      let sendResp = function (isSet) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isSet: isSet
        }
        debugLog('checkPushPref result', response)
        app.ports.javascriptInPort.send(response)
      }
      if (window.localStorage.getItem(PUSH_PREF) === null) {
        sendResp(false)
      } else {
        sendResp(true)
      }
      break
    }
    case 'disablePushPref': {
      debugLog('=====================', 'disablePushPref')
      window.localStorage.removeItem(PUSH_PREF)
      pushSub.unsubscribeFromPush()
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        isSet: false
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'downloadAuthPdfFromRegistration': {
      debugLog('=======================', 'downloadAuthPdfFromRegistration')
      const accountName = arg.data.accountName
      const passphrase = arg.data.passphrase
      downloadPdf(
        accountName,
        passphrase,
        arg.responseAddress,
        arg.responseData
      )
      break
    }
    case 'downloadAuthPdfFromProfile': {
      debugLog('=======================', 'downloadAuthPdfFromProfile')
      const store = JSON.parse(window.localStorage.getItem(USER_KEY))
      const pin = arg.data.pin

      // `.encryptedPassphrase` property was added in https://github.com/cambiatus/frontend/pull/270 while redesigning
      // the Profile page. For the users who were already logged-in before these changes were introduced,
      // this property may not exist. This case is handled by passing `isDownloaded: false` to Elm
      // for further processing.
      if (store.encryptedPassphrase) {
        const decryptedPassphrase = sjcl.decrypt(pin, store.encryptedPassphrase)
        downloadPdf(
          store.accountName,
          decryptedPassphrase,
          arg.responseAddress,
          arg.responseData
        )
      } else {
        // The case when there's not passphrase stored in user's browser, only the Private Key
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isDownloaded: false
        }

        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'accountNameToUint64': {
      debugLog('=======================', 'accountNameToUint64')
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        uint64name: eos.modules.format.encodeName(arg.data.accountName, false)
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'scrollIntoView': {
      debugLog('=======================', 'scrollIntoView')
      document.getElementById(arg.data.id).scrollIntoView(true)
      break
    }
    case 'validateDeadline': {
      debugLog('=============================', 'validatingDate')

      const parsedDate = new Date(arg.data.deadline)
      const now = new Date()

      console.log('p', parsedDate)
      if (parsedDate.toString() === 'Invalid Date' || parsedDate < now) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          error: parsedDate
        }
        app.ports.javascriptInPort.send(response)
        break
      } else {
        const isoDate = parsedDate.toISOString()

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          date: isoDate
        }
        app.ports.javascriptInPort.send(response)
        break
      }
    }
    case 'hideFooter': {
      debugLog('======================', 'hideFooter')
      document.getElementById('guest-footer').className += ' guest__footer'
      break
    }
    case 'subscribeToNewCommunity': {
      debugLog('=======================', 'newCommunitySubscription')
      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      debugLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      const onStart = data => {
        const payload = { dta: data, msg: 'starting community subscription' }
        debugLog('==========================', payload)
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      const onAbort = data => {
        debugLog('===========================', 'aborting community subscription')
      }

      const onCancel = data => {
        debugLog(
          '===========================',
          'cancellling community subscription '
        )
      }

      const onError = data => {
        debugLog('community subscrition error', data)
      }

      let onResult = data => {
        debugLog('===========================', 'community subscription results')
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
      break
    }
    case 'subscribeToTransfer': {
      debugLog('=======================', 'subscribeToTransfer')

      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      debugLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      let onStart = data => {
        const payload = { dta: data, msg: 'starting transfer subscription' }
        debugLog('==========================', payload)

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      const onAbort = data => {
        debugLog('===========================', 'aborting transfer subscription')
      }

      const onCancel = data => {
        debugLog('===========================', 'cancel transfer subscription ')
      }

      const onError = data => {
        debugLog('transfer subscrition error', data)
      }

      const onResult = data => {
        debugLog('===========================', 'Transfer subscription results')
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

      break
    }
    case 'subscribeToUnreadCount': {
      debugLog('=======================', 'unreadCountSubscription')
      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      debugLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      const onStart = data => {
        const payload = { dta: data, msg: 'starting unread countsubscription' }
        debugLog('==========================', payload)
      }

      const onAbort = data => {
        debugLog(
          '===========================',
          'aborting unread count subscription'
        )
      }

      const onCancel = data => {
        debugLog(
          '===========================',
          'cancelling unread count subscription '
        )
      }

      const onError = data => {
        debugLog('community subscrition error', data)
      }

      const onResult = data => {
        debugLog(
          '===========================',
          'unread count subscription results'
        )
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
      break
    }
    case 'copyToClipboard': {
      debugLog('=======================', 'copyToClipboard')
      document.querySelector('#' + arg.data.id).select()
      document.execCommand('copy')
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'setSelectedCommunity': {
      debugLog('=======================', 'storeSelectedCommunity')

      Sentry.addBreadcrumb({
        type: 'navigation',
        category: 'navigation',
        data: {
          from: window.localStorage.getItem(SELECTED_COMMUNITY_KEY),
          to: arg.data.selectedCommunity
        },
        message: 'Changed to community ' + arg.data.selectedCommunity,
        level: Sentry.Severity.Info
      })

      window.localStorage.removeItem(SELECTED_COMMUNITY_KEY)
      window.localStorage.setItem(
        SELECTED_COMMUNITY_KEY,
        arg.data.selectedCommunity
      )

      break
    }
    default: {
      debugLog('No treatment found for ', arg.data.name)
    }
  }
}
