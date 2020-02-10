import ScatterJS from 'scatterjs-core'
import ScatterEOS from 'scatterjs-plugin-eosjs'
import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import sjcl from 'sjcl'
import axios from 'axios'
import * as JsPdf from 'jspdf'
import './styles/processed.css'
import mnemonic from './scripts/mnemonic.js'
import configuration from './scripts/config.js'
import registerServiceWorker from './scripts/registerServiceWorker'
import * as pushSub from './scripts/pushNotifications'
import { Elm } from './elm/Main.elm'
import * as Sentry from '@sentry/browser'
import * as AbsintheSocket from '@absinthe/socket'
const { Socket: PhoenixSocket } = require('phoenix')

// =========================================
// App startup
// =========================================

ScatterJS.plugins(new ScatterEOS())
let eos = null
let scatter = null
var isAuthenticated = false // eslint-disable-line
const USER_KEY = 'bespiral.user'
const LANGUAGE_KEY = 'bespiral.language'
const AUTH_PREF_KEY = 'bespiral.auth.pref'
const CHAT_TOKEN_KEY = 'bespiral.chat.token'
const CHAT_USER_ID_KEY = 'bespiral.chat.user.id'
const CHAT_CONTAINER_KEY = 'bespiral.chat.container'
const PUSH_PREF = 'bespiral.push.pref'
const env = process.env.NODE_ENV || 'development'
const config = configuration[env]
const urlParams = new URLSearchParams(window.location.search)
const langParam = urlParams.get('lang')
let isTrackingChat = false

function flags () {
  const user = JSON.parse(window.localStorage.getItem(USER_KEY))
  var flags_ = {
    env: env,
    endpoints: config.endpoints,
    language:
      langParam ||
      window.localStorage.getItem(LANGUAGE_KEY) ||
      navigator.language ||
      navigator.userLanguage ||
      'en-US',
    accountName: (user && user.accountName) || null,
    isPinAvailable: !!(user && user.encryptedKey),
    authPreference: window.localStorage.getItem(AUTH_PREF_KEY),
    logo: config.logo,
    logoMobile: config.logoMobile,
    now: Date.now(),
    allowCommunityCreation: config.allowCommunityCreation
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

// STORE CHAT USER ID

function storeChatUserId (userId) {
  window.localStorage.setItem(CHAT_USER_ID_KEY, userId)
}

// STORE CHAT TOKEN

function storeChatToken (token) {
  window.localStorage.setItem(CHAT_TOKEN_KEY, token)
}

// STORE CHAT CONTAINER

function storeChatContainer (container) {
  window.localStorage.setItem(CHAT_CONTAINER_KEY, container)
}

// STORE PUSH PREF

function storePushPref (pref) {
  window.localStorage.setItem(PUSH_PREF, pref)
}

// GET CHAT TOKEN

function getChatToken () {
  return window.localStorage.getItem(CHAT_TOKEN_KEY)
}

// GET CHAT CONTAINER

function getChatContainer () {
  return window.localStorage.getItem(CHAT_CONTAINER_KEY)
}

// POST MESSAGE TO ROCKET.CHAT

function sendToChat (iframeChat, message, maybeTargetOrigin) {
  const targetOrigin = maybeTargetOrigin || '*'

  iframeChat.contentWindow.postMessage(message, targetOrigin)
}

// LOGOUT FROM ROCKET.CHAT

function attemptToLogout (container) {
  const c = container || getChatContainer()

  if (c) {
    isTrackingChat = false
    const iframeChat = document.getElementById(container)
    const message = {
      externalCommand: 'logout'
    }
    sendToChat(iframeChat, message)
    window.removeEventListener('message', window.onchat)
  }
}

// LOGIN INTO ROCKET.CHAT

function attemptToLogin (container, token, address, addressData) {
  const c = container || getChatContainer()
  const t = token || getChatToken()

  if (c && t) {
    const chatUrl = config.endpoints.chat
    const iframeChat = document.getElementById(c)
    iframeChat.src = `${chatUrl}`

    if (!isTrackingChat) {
      isTrackingChat = true

      window.onchat = function (event) {
        if (event.data.eventName === 'startup') {
          devLog('========================', 'onStartup:')
          devLog('onStartup:', event.data)
          const message = {
            event: 'login-with-token',
            loginToken: t
          }
          sendToChat(iframeChat, message)
        } else if (
          chatUrl.indexOf(event.origin) >= 0 &&
          event.data.eventName === 'notification'
        ) {
          devLog('========================', 'onNotification')
          devLog('onNotification', event.data)

          const username =
            event &&
            event.data &&
            event.data.data &&
            event.data.data.notification &&
            event.data.data.notification.payload &&
            event.data.data.notification.payload.sender &&
            event.data.data.notification.payload.sender.username

          if (username) {
            const response = {
              address: address,
              addressData: addressData,
              username: username
            }
            app.ports.javascriptInPort.send(response)
          } else {
            const errorResponse = {
              address: address,
              addressData: addressData,
              error: 'No username found'
            }
            app.ports.javascriptInPort.send(errorResponse)
          }
        }
      }

      window.addEventListener('message', window.onchat)
    }
  }
}

// CREATE CHAT INSTANCE

function createChat (username) {
  const chatUrl = config.endpoints.chat
  window.open(`${chatUrl}/direct/${username}`)
}

// STORE PIN

async function storePin (data, pin) {
  // encrypt key using PIN
  const hashedKey = ecc.sha256(data.privateKey)
  const encryptedKey = sjcl.encrypt(pin, data.privateKey)
  var storeData = {
    accountName: data.accountName,
    encryptedKey: encryptedKey,
    encryptedKeyIntegrityCheck: hashedKey
  }

  window.localStorage.removeItem(USER_KEY)
  window.localStorage.setItem(USER_KEY, JSON.stringify(storeData))
}

app.ports.javascriptOutPort.subscribe(handleJavascriptPort)
async function handleJavascriptPort (arg) {
  devLog('handleJavascriptPort', arg)
  switch (arg.data.name) {
    case 'checkScatterAvailability': {
      devLog('=========================', 'checkScatterAvailability')
      var sendScatterResponse = function (isAvailable) {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isAvailable: isAvailable
        }
        devLog('checkScatterAvailability response', response)
        app.ports.javascriptInPort.send(response)
      }
      ScatterJS.scatter
        .connect(
          'cambiatus',
          {
            initTimeout: 2500
          }
        )
        .then(function (connected) {
          if (connected) {
            scatter = ScatterJS.scatter
            window.ScatterJS = null
            sendScatterResponse(true)
          } else {
            sendScatterResponse(false)
          }
        })
        .catch(error => {
          devLog('checkScatterAvailability error', error)
          sendScatterResponse(false)
        })
      break
    }
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
      const [randomWords, hexRandomWords] = mnemonic.generateRandom()
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

          storePin(
            Object.assign(data, {
              accountName: arg.data.account,
              privateKey: privateKey
            }),
            arg.data.pin
          )

          isAuthenticated = true
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
    case 'loginWithScatter': {
      devLog('=========================', 'loginWithScatter')
      if (scatter) {
        scatter
          .suggestNetwork(config.network)
          .then(function () {
            scatter
              .getIdentity({
                personal: ['firstname', 'lastname', 'email'],
                accounts: [config.network]
              })
              .then(function (identity) {
                devLog('identity', identity)
                eos = scatter.eos(config.network, Eos, config.eosOptions)
                isAuthenticated = true
                storeAccountName(identity.accounts[0].name)
                storeAuthPreference('scatter')
                const response = {
                  address: arg.responseAddress,
                  addressData: arg.responseData,
                  accountName: identity.accounts[0].name,
                  firstName: identity.personal.firstname,
                  lastName: identity.personal.lastname,
                  email: identity.personal.email,
                  publicKey: identity.publicKey
                }
                devLog('response', response)
                app.ports.javascriptInPort.send(response)
              })
              .catch(function (error) {
                const response = {
                  address: arg.responseAddress,
                  addressData: arg.responseData,
                  isAvailable: true,
                  error: error.message
                }
                devLog('response', response)
                app.ports.javascriptInPort.send(response)
              })
          })
          .catch(function (error) {
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              isAvailable: true,
              error: error.message
            }
            devLog('response', response)
            app.ports.javascriptInPort.send(response)
          })
      } else {
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          isAvailable: false,
          error: 'Scatter is unavailable'
        }
        devLog('response', response)
        app.ports.javascriptInPort.send(response)
      }
      break
    }
    case 'loginWithPrivateKey': {
      devLog('=========================', 'loginWithPrivateKey')
      var form = arg.data.form
      var privateKey = arg.data.form.privateKey
      privateKey =
        privateKey.split(' ').length === 12
          ? ecc.seedPrivate(mnemonic.toSeedHex(form.privateKey))
          : form.privateKey

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
      var loginForm = arg.data.form
      const accountName = arg.data.accountName

      // check if it is actually 12 words and convert it
      privateKey =
        loginForm.privateKey.split(' ').length === 12
          ? ecc.seedPrivate(mnemonic.toSeedHex(loginForm.privateKey))
          : loginForm.privateKey

      privateKey = loginForm.privateKey
      if (loginForm.privateKey.split(' ').length > 1) {
        privateKey = ecc.seedPrivate(mnemonic.toSeedHex(loginForm.privateKey))
      }

      if (loginForm.usePin) {
        storePin(
          {
            accountName: accountName,
            privateKey: privateKey
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
      isAuthenticated = true
      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        accountName: accountName,
        privateKey: loginForm.privateKey
      }
      devLog('response', response)
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

          isAuthenticated = true
          storeAuthPreference('pin')
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            accountName: store.accountName,
            privateKey: decryptedKey
          }
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
    case 'chatCredentials': {
      devLog('=========================', 'chatCredentials')
      storeChatUserId(arg.data.credentials.chatUserId)
      storeChatToken(arg.data.credentials.chatToken)
      storeChatContainer(arg.data.container)
      attemptToLogin(
        arg.data.container,
        arg.data.credentials.chatToken,
        arg.data.notificationAddress,
        arg.responseData
      )
      break
    }
    case 'openChat': {
      devLog('=========================', 'openChat')
      devLog('openChat: ', arg.data)
      createChat(arg.data.username)
      break
    }
    case 'logout': {
      devLog('=========================', 'logout')
      window.localStorage.removeItem(USER_KEY)
      window.localStorage.removeItem(AUTH_PREF_KEY)
      window.localStorage.removeItem(CHAT_USER_ID_KEY)
      window.localStorage.removeItem(CHAT_TOKEN_KEY)
      attemptToLogout(arg.data.container)
      if (scatter) {
        scatter.forgetIdentity()
      }
      isAuthenticated = false
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
    case 'printAuthPdf': {
      devLog('=======================', 'printAuthPdf')
      const filename = '12_Words.pdf'
      let words = document.getElementById('12__words').textContent
      let pkey = document.getElementById('p__key').textContent

      var doc = new JsPdf()
      doc.text('Your Words', 20, 20)
      doc.text(words, 20, 30)
      doc.text('Your Key', 20, 40)
      doc.text(pkey, 20, 50)
      doc.save(filename)

      const response = {
        address: arg.responseAddress,
        addressData: arg.responseData,
        isDownloaded: true
      }
      app.ports.javascriptInPort.send(response)
      break
    }
    case 'validateDeadline': {
      devLog('=============================', 'validatingDate')

      const parsedDate = new Date(arg.data.deadline)
      const now = new Date()

      console.log('p', parsedDate)
      if (parsedDate.toString() === ('Invalid Date') || (parsedDate < now)) {
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

      let onStart = (data) => {
        const payload = { dta: data, msg: 'starting community subscription' }
        devLog('==========================', payload)
        const response = {
          address: arg.responseAddress,
          addressData: arg.responseData,
          state: 'starting'
        }
        app.ports.javascriptInPort.send(response)
      }

      let onAbort = (data) => {
        devLog('===========================', 'aborting community subscription')
      }

      let onCancel = (data) => {
        devLog('===========================', 'cancellling community subscription ')
      }

      let onError = (data) => {
        devLog('community subscrition error', data)
      }

      let onResult = (data) => {
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

      let onStart = (data) => {
        const payload = { dta: data, msg: 'starting unread countsubscription' }
        devLog('==========================', payload)
      }

      let onAbort = (data) => {
        devLog('===========================', 'aborting unread count subscription')
      }

      let onCancel = (data) => {
        devLog('===========================', 'cancelling unread count subscription ')
      }

      let onError = (data) => {
        devLog('community subscrition error', data)
      }

      let onResult = (data) => {
        devLog('===========================', 'unread count subscription results')
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
    default: {
      devLog('No treatment found for ', arg.data.name)
    }
  }
}
