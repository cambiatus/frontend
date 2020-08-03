import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import sjcl from 'sjcl'
import axios from 'axios'
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

  return urlParams.get('lang') ||
    window.localStorage.getItem(LANGUAGE_KEY) ||
    navigator.language ||
    navigator.userLanguage ||
    'en-US'
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
    selectedCommunity: getSelectedCommunity() || config.selectedCommunity
  }
  devLog('flags', flags_)
  return flags_
}

// Start elm app with flags
const app = Elm.Main.init({
  flags: flags()
})

// Register Service Worker After App
registerServiceWorker()

// Log

function devLog (name, arg) {
  if (env === 'development') {
    console.log('[dev]', name, arg)
  }
}

// Init Sentry
Sentry.init({
  dsn: 'https://535b151f7b8c48f8a7307b9bc83ebeba@sentry.io/1480468'
})

// Sentry Reporter
function sentryReporter (msg, exception) {
  Sentry.withScope(scope => {
    scope.setExtra(msg)
    scope.setTag('event', msg)
    scope.setLevel('error')
    Sentry.captureException(exception)
  })
}

app.ports.logError.subscribe((msg, err) => {
  if (env === 'development') {
    console.error(msg, err)
  } else {
    sentryReporter(msg, err)
  }
})

app.ports.logDebug.subscribe(devLog)

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
  const getLocalFontUrl = filename => window.location.origin + '/fonts/' + filename

  pdfMake.fonts = {
    Gotham: {
      normal: getLocalFontUrl('gotham-rounded-book.otf'),
      bold: getLocalFontUrl('opensans-bold-no-ligatures.ttf')
    }
  }

  const definition = pdfDefinition(passphrase)
  const pdf = pdfMake.createPdf(definition)

  pdf.download(accountName + '_cambiatus.pdf')

  const response = {
    address: responseAddress,
    addressData: responseData,
    isDownloaded: true
  }

  app.ports.javascriptInPort.send(response)
}

app.ports.javascriptOutPort.subscribe(handleJavascriptPort)
async function handleJavascriptPort (arg) {
  devLog('handleJavascriptPort', arg)
  switch (arg.data.name) {
    case 'checkAccountAvailability': {
      devLog('=========================', 'checkAccountAvailability')
      var sendResponse = function (isAvailable) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isAvailable: isAvailable
        }
        devLog('checkAccountAvailability response', response)
        app.ports.javascriptInPort.send(response)
      }
      eos
        .getAccount(arg.data.accountName)
        .then(_ => sendResponse(false))
        .catch(e => {
          // Invalid name exception
          devLog('checkAccountAvailability', e)
          if (JSON.parse(e.message).error.code === 3010001) {
            sendResponse(false)
          } else {
            sendResponse(true)
          }
        })
      break
    }
    case 'generateAccount': {
      devLog('=========================', 'generateAccount')
      const userLang = getUserLanguage()
      const [randomWords, hexRandomWords] = mnemonic.generateRandom(userLang)
      const privateKey = ecc.seedPrivate(hexRandomWords)
      const publicKey = ecc.privateToPublic(privateKey)
      const data = {
        account: arg.data.account,
        ownerKey: publicKey,
        activeKey: publicKey
      }

      if (arg.data.invitationId) {
        data.invitationId = arg.data.invitationId
      }

      axios
        .post(config.endpoints.api + '/api/chain/account', data)
        .then(res => {
          eos = Eos(
            Object.assign(config.eosOptions, {
              keyProvider: privateKey
            })
          )

          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            data: {
              ownerKey: publicKey,
              activeKey: publicKey,
              accountName: res.data.account,
              transactionId: res.data.transaction_id,
              words: randomWords,
              privateKey: privateKey
            }
          }
          devLog('generateAccount succeed', res)
          devLog('generateAccount response', response)
          app.ports.javascriptInPort.send(response)
        })
        .catch(e => {
          devLog('generateAccount failed', e)
          var errorResponse = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: e
          }
          devLog('generateAccount response', errorResponse)
          app.ports.javascriptInPort.send(errorResponse)
        })
      break
    }
    case 'loginWithPrivateKey': {
      devLog('=========================', 'loginWithPrivateKey')
      const passphrase = arg.data.form.passphrase
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

      if (ecc.isValidPrivate(privateKey)) {
        const publicKey = ecc.privateToPublic(privateKey)
        const accounts = await eos.getKeyAccounts(publicKey)
        const user = JSON.parse(window.localStorage.getItem(USER_KEY))
        devLog('loginWithPrivateKey:accounts', accounts)

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
          devLog('response', response)
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
          devLog('response', response)
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
          devLog('response', response)
          app.ports.javascriptInPort.send(response)
        }
      } else {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          error: 'Invalid key'
        }
        devLog('response', response)
        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'loginWithPrivateKeyAccount': {
      devLog('========================', 'loginWithPrivateKeyAccount')
      const loginForm = arg.data.form
      const accountName = arg.data.accountName
      const passphrase = loginForm.passphrase
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

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
      eos = Eos(
        Object.assign(config.eosOptions, {
          keyProvider: privateKey
        })
      )
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        accountName: accountName,
        privateKey: privateKey
      }
      devLog('response', response)
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'changePin': {
      devLog('========================', 'changePin')

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
        data.passphrase = sjcl.decrypt(currentPin, userStorage.encryptedPassphrase)
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
      devLog('========================', 'loginWithPin')
      const store = JSON.parse(window.localStorage.getItem(USER_KEY))
      const pin = arg.data.pin
      if (store && store.encryptedKey && store.accountName) {
        try {
          const decryptedKey = sjcl.decrypt(pin, store.encryptedKey)

          eos = Eos(
            Object.assign(config.eosOptions, {
              keyProvider: decryptedKey
            })
          )

          storeAuthPreference('pin')
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            accountName: store.accountName,
            privateKey: decryptedKey
          }

          // Set default selected community
          window.localStorage.setItem(
            SELECTED_COMMUNITY_KEY,
            flags().selectedCommunity
          )

          devLog('response', response)
          app.ports.javascriptInPort.send(response)
        } catch (e) {
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: 'Invalid PIN'
          }
          devLog('response', response)
          app.ports.javascriptInPort.send(response)
        }
      } else {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          error: 'PIN is unavailable'
        }
        devLog('response', response)
        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'eosTransaction': {
      devLog('=========================', 'transaction')
      devLog('DATA', arg.data)
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
          devLog('eos.transaction.succeed', res)
          devLog('response', response)
          app.ports.javascriptInPort.send(response)
        })
        .catch(error => {
          var errorResponse = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            error: error
          }
          devLog('eos.transaction.failed', errorResponse)
          // Send to sentry
          Sentry.configureScope(scope => {
            scope.setTag('type', 'eos-transaction')
            scope.setExtra('data', arg.data)
            Sentry.setExtra('response', errorResponse)
            Sentry.setExtra('error', errorResponse.error)
            Sentry.captureMessage('EOS Error')
          })
          app.ports.javascriptInPort.send(errorResponse)
        })
      break
    }
    case 'logout': {
      devLog('=========================', 'logout')
      window.localStorage.removeItem(USER_KEY)
      window.localStorage.removeItem(AUTH_PREF_KEY)
      window.localStorage.removeItem(SELECTED_COMMUNITY_KEY)
      break
    }
    case 'requestPushPermission': {
      devLog('======================', 'requestingPushPermissions')
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
            devLog('requestPushPermission response', response)
            app.ports.javascriptInPort.send(response)
          })
          .catch(err => devLog('Push Permission Denied', err))
      } else {
        devLog('=======================', 'Push not supported on this agent')
      }
      break
    }
    case 'completedPushUpload': {
      devLog('=====================', 'cachingPushSubscription')
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
      devLog('=====================', 'checkingPushPref')
      let sendResp = function (isSet) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isSet: isSet
        }
        devLog('checkPushPref result', response)
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
      devLog('=====================', 'disablePushPref')
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
      devLog('=======================', 'downloadAuthPdfFromRegistration')
      const accountName = arg.data.accountName
      const passphrase = arg.data.passphrase
      downloadPdf(accountName, passphrase, arg.responseAddress, arg.responseData)
      break
    }
    case 'downloadAuthPdfFromProfile': {
      devLog('=======================', 'downloadAuthPdfFromProfile')
      const store = JSON.parse(window.localStorage.getItem(USER_KEY))
      const pin = arg.data.pin

      // `.encryptedPassphrase` property was added in https://github.com/cambiatus/frontend/pull/270 while redesigning
      // the Profile page. For the users who were already logged-in before these changes were introduced,
      // this property may not exist. This case is handled by passing `isDownloaded: false` to Elm
      // for further processing.
      if (store.encryptedPassphrase) {
        const decryptedPassphrase = sjcl.decrypt(pin, store.encryptedPassphrase)
        downloadPdf(store.accountName, decryptedPassphrase, arg.responseAddress, arg.responseData)
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
    case 'validateDeadline': {
      devLog('=============================', 'validatingDate')

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
      devLog('======================', 'hideFooter')
      document.getElementById('guest-footer').className += ' guest__footer'
      break
    }
    case 'subscribeToNewCommunity': {
      devLog('=======================', 'newCommunitySubscription')
      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      devLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      let onStart = data => {
        const payload = { dta: data, msg: 'starting community subscription' }
        devLog('==========================', payload)
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      let onAbort = data => {
        devLog('===========================', 'aborting community subscription')
      }

      let onCancel = data => {
        devLog(
          '===========================',
          'cancellling community subscription '
        )
      }

      let onError = data => {
        devLog('community subscrition error', data)
      }

      let onResult = data => {
        devLog('===========================', 'community subscription results')
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
      devLog('=======================', 'subscribeToTransfer')

      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      devLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      let onStart = data => {
        const payload = { dta: data, msg: 'starting transfer subscription' }
        devLog('==========================', payload)

        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      let onAbort = data => {
        devLog('===========================', 'aborting transfer subscription')
      }

      let onCancel = data => {
        devLog('===========================', 'cancel transfer subscription ')
      }

      let onError = data => {
        devLog('transfer subscrition error', data)
      }

      let onResult = data => {
        devLog('===========================', 'Transfer subscription results')
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
      devLog('=======================', 'unreadCountSubscription')
      let notifiers = []

      // Open a socket connection
      const socketConn = new PhoenixSocket(config.endpoints.socket)

      // Build a graphql Socket
      const abSocket = AbsintheSocket.create(socketConn)

      // Remove existing notifiers if any
      notifiers.map(notifier => AbsintheSocket.cancel(abSocket, notifier))

      devLog('subscription doc', arg.data.subscription)
      // Create new notifiers
      notifiers = [arg.data.subscription].map(operation =>
        AbsintheSocket.send(abSocket, {
          operation,
          variables: {}
        })
      )

      let onStart = data => {
        const payload = { dta: data, msg: 'starting unread countsubscription' }
        devLog('==========================', payload)
      }

      let onAbort = data => {
        devLog(
          '===========================',
          'aborting unread count subscription'
        )
      }

      let onCancel = data => {
        devLog(
          '===========================',
          'cancelling unread count subscription '
        )
      }

      let onError = data => {
        devLog('community subscrition error', data)
      }

      let onResult = data => {
        devLog(
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
      devLog('=======================', 'copyToClipboard')
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
      devLog('=======================', 'storeSelectedCommunity')

      window.localStorage.removeItem(SELECTED_COMMUNITY_KEY)
      window.localStorage.setItem(
        SELECTED_COMMUNITY_KEY,
        arg.data.selectedCommunity
      )

      break
    }
    default: {
      devLog('No treatment found for ', arg.data.name)
    }
  }
}
