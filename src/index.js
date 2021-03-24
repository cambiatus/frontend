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

const mainApp = () => {
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
  const AUTH_TOKEN = 'bespiral.auth_token'
  const RECENT_SEARCHES = 'bespiral.recent_search'
  const env = process.env.NODE_ENV || 'development'
  const graphqlSecret = process.env.GRAPHQL_SECRET || ''
  const config = configuration[env]

  const operationsBeforeLoad = []

  let setItem = (key, value) => {
    operationsBeforeLoad.push({ method: 'set', key, value })
    window.localStorage.setItem(key, value)
  }

  let getItem = (key) => window.localStorage.getItem(key)

  let removeItem = (key) => {
    operationsBeforeLoad.push({ method: 'remove', key })
    window.localStorage.removeItem(key)
  }

  const setupIframe = (isInitial) => {
    const iframe = document.createElement('iframe')
    const src = config.endpoints.globalStorage

    iframe.onload = () => {
      const contentWindow = iframe.contentWindow
      const postMessage = (message) => {
        contentWindow.postMessage(message, src)
      }

      setItem = (key, value) => {
        postMessage({
          method: 'set',
          key,
          value
        })
        window.localStorage.setItem(key, value)
      }

      removeItem = (key) => {
        postMessage({
          method: 'remove',
          key
        })
        window.localStorage.removeItem(key)
      }

      operationsBeforeLoad.forEach((operation) => {
        if (operation.method === 'set') {
          setItem(operation.key, operation.value)
        } else if (operation.method === 'remove') {
          removeItem(operation.key)
        } else {
          postMessage(operation)
        }
      })

      if (isInitial) {
        postMessage({
          method: 'getMany',
          keys: [USER_KEY, LANGUAGE_KEY, AUTH_PREF_KEY, PUSH_PREF, SELECTED_COMMUNITY_KEY, AUTH_TOKEN, RECENT_SEARCHES]
        })
      }
    }

    iframe.src = src
    iframe.style = 'display: none'
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
        }
      })

      if (!hasRunMain) {
        main()
      }
    }
  }

  // Read globalStorage data on load
  window.onload = () => {
    setupIframe(true)
  }

  const main = () => {
    hasRunMain = true
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

    function flags () {
      const user = JSON.parse(getItem(USER_KEY))
      return {
        env: env,
        graphqlSecret: graphqlSecret,
        endpoints: config.endpoints,
        language: getUserLanguage(),
        accountName: (user && user.accountName) || null,
        isPinAvailable: !!(user && user.encryptedKey),
        authPreference: getItem(AUTH_PREF_KEY),
        authToken: getItem(AUTH_TOKEN),
        logo: config.logo,
        logoMobile: config.logoMobile,
        now: Date.now(),
        allowCommunityCreation: config.allowCommunityCreation,
        tokenContract: config.tokenContract,
        communityContract: config.communityContract
      }
    }

    // Start elm app with flags
    const app = Elm.Main.init({
      flags: flags()
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

    // Ports error Reporter
    app.ports.logError.subscribe((msg, err) => {
      Sentry.addBreadcrumb({
        message: 'Begin Elm Error port javascript handler',
        level: Sentry.Severity.Info,
        type: 'debug',
        category: 'started'
      })
      if (env === 'development') {
        console.error(msg, err)
      } else {
        let error = 'Generic Elm Error port msg'
        let details = ''

        if (Object.prototype.toString.call(msg) === '[object Array]') {
          [error, details] = msg
        }

        Sentry.withScope(scope => {
          scope.setTag('type', 'elm-error')
          scope.setLevel(Sentry.Severity.Error)
          scope.setExtra('Error shared by Elm', err)
          scope.setExtra('raw msg', msg)
          scope.setExtra('Parsed details', details)
          Sentry.captureMessage(error + details)
        })
      }
      Sentry.addBreadcrumb({
        message: 'Ended Elm Error port javascript handler',
        level: Sentry.Severity.Info,
        type: 'debug',
        category: 'ended'
      })
    })

    app.ports.logDebug.subscribe(debugLog)

    // =========================================
    // EOS / Identity functions
    // =========================================

    eos = Eos(config.eosOptions)

    // STORE LANGUAGE

    app.ports.storeLanguage.subscribe(storeLanguage)

    function storeLanguage (lang) {
      setItem(LANGUAGE_KEY, lang)
    }

    // STORE RECENT SEARCHES
    app.ports.storeRecentSearches.subscribe(query =>
      setItem(RECENT_SEARCHES, query)
    )

    // RETRIEVE RECENT SEARCHES
    app.ports.getRecentSearches.subscribe(() => {
      app.ports.gotRecentSearches.send(getItem(RECENT_SEARCHES) || '[]')
    })

    // STORE AUTH PREFERENCE

    function storeAuthPreference (auth) {
      setItem(AUTH_PREF_KEY, auth)
    }

    app.ports.storeAuthToken.subscribe(token =>
      setItem(AUTH_TOKEN, token)
    )

    // STORE ACCOUNTNAME

    function storeAccountName (accountName) {
      var storeData = {
        accountName: accountName
      }
      removeItem(USER_KEY)
      setItem(USER_KEY, JSON.stringify(storeData))
    }

    // STORE PUSH PREF

    function storePushPref (pref) {
      setItem(PUSH_PREF, pref)
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

      removeItem(USER_KEY)
      setItem(USER_KEY, JSON.stringify(storeData))
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
      switch (arg.data.name) {
        case 'checkAccountAvailability': {
          debugLog('checkAccountAvailability', '')
          var sendResponse = function (isAvailable, error) {
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              isAvailable: isAvailable,
              error: error
            }
            debugLog('checkAccountAvailability port finished', response)
            app.ports.javascriptInPort.send(response)
          }
          eos
            .getAccount(arg.data.account)
            .then(_ => sendResponse(false))
            .catch(e => {
              // Invalid name exception
              if (JSON.parse(e.message).error.code === 3010001) {
                debugLog('checkAccountAvailability port failed', e)
                sendResponse(false)
              } else {
                sendResponse(true)
              }
            })
          break
        }
        case 'generateKeys': {
          debugLog('generateKeys port started', '')
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

          debugLog('generateKeys port finished', response)
          app.ports.javascriptInPort.send(response)
          break
        }
        case 'loginWithPrivateKey': {
          debugLog('loginWithPrivateKey', '')
          const passphrase = arg.data.form.passphrase
          const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

          if (ecc.isValidPrivate(privateKey)) {
            const publicKey = ecc.privateToPublic(privateKey)
            const accounts = await eos.getKeyAccounts(publicKey)
            const user = JSON.parse(getItem(USER_KEY))
            debugLog('loginWithPrivateKey port accounts', accounts)

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
              debugLog('loginWithPrivateKey port failed', response)
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
              debugLog('loginWithPrivateKey port failed', response)
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
              debugLog('loginWithPrivateKey port finished', response)
              app.ports.javascriptInPort.send(response)
            }
          } else {
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              error: 'Invalid key'
            }
            debugLog('loginWithPrivateKey port failed', response)
            app.ports.javascriptInPort.send(response)
          }
          break
        }
        case 'loginWithPrivateKeyAccount': {
          debugLog('loginWithPrivateKeyAccount port started', '')
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
          debugLog('loginWithPrivateKeyAccount port finished', response)
          app.ports.javascriptInPort.send(response)
          break
        }
        case 'changePin': {
          debugLog('changePin port started', '')

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
          debugLog('loginWithPin port started', '')
          const store = JSON.parse(getItem(USER_KEY))
          const pin = arg.data.pin
          if (store && store.encryptedKey && store.accountName) {
            try {
              const decryptedKey = sjcl.decrypt(pin, store.encryptedKey)

              eos = Eos(Object.assign(config.eosOptions, { keyProvider: decryptedKey }))

              // Configure Sentry logged user
              Sentry.setUser({ account: store.accountName })

              storeAuthPreference('pin')

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

              debugLog('loginWithPin port finished', response)
              app.ports.javascriptInPort.send(response)
            } catch (e) {
              const response = {
                address: arg.responseAddress,
                addressData: arg.responseData,
                error: 'Invalid PIN'
              }
              debugLog('loginWithPin port failed', response)
              app.ports.javascriptInPort.send(response)
            }
          } else {
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              error: 'PIN is unavailable'
            }
            debugLog('loginWithPin port failed', response)
            app.ports.javascriptInPort.send(response)
          }
          break
        }
        case 'eosTransaction': {
          debugLog('eosTransaction port started', arg.data)

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

              debugLog('eosTransaction port response', response)
              app.ports.javascriptInPort.send(response)
            })
            .catch(errorString => {
              const error = JSON.parse(errorString)
              const errorResponse = {
                address: arg.responseAddress,
                addressData: arg.responseData,
                error: error
              }
              debugLog('eosTransaction port failed', errorResponse)

              // Send to sentry
              Sentry.addBreadcrumb({
                type: 'default',
                category: 'sentry.transaction',
                level: 'info',
                message: 'Failure pushing transaction to EOS'
              })
              Sentry.withScope(scope => {
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
          debugLog('logout port started', '')
          removeItem(USER_KEY)
          removeItem(AUTH_PREF_KEY)
          removeItem(SELECTED_COMMUNITY_KEY)
          removeItem(AUTH_TOKEN)
          Sentry.addBreadcrumb({
            category: 'auth',
            message: 'User logged out'
          })
          Sentry.setUser(null)
          break
        }
        case 'requestPushPermission': {
          debugLog('requestingPushPermissions port started', '')
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
                debugLog('requestPushPermission port ended', response)
                app.ports.javascriptInPort.send(response)
              })
              .catch(err => debugLog('requestPushPermission port error: Push Permission Denied', err))
          } else {
            debugLog('requestPushPermission port error: Push not supported on this agent', '')
          }
          break
        }
        case 'completedPushUpload': {
          debugLog('cachingPushSubscription port started', '')
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
          debugLog('checkingPushPref port started', '')
          let sendResponse = function (isSet) {
            const response = {
              address: arg.responseAddress,
              addressData: arg.responseData,
              isSet: isSet
            }
            debugLog('checkPushPref port ended', response)
            app.ports.javascriptInPort.send(response)
          }

          sendResponse(getItem(PUSH_PREF) !== null)
          break
        }
        case 'disablePushPref': {
          debugLog('disablePushPref port started', '')
          removeItem(PUSH_PREF)
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
          debugLog('downloadAuthPdfFromRegistration port started', '')
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
          debugLog('downloadAuthPdfFromProfile port started', '')
          const store = JSON.parse(getItem(USER_KEY))
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
          debugLog('accountNameToUint64 port started', '')
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData,
            uint64name: eos.modules.format.encodeName(arg.data.accountName, false)
          }
          app.ports.javascriptInPort.send(response)
          break
        }
        case 'scrollIntoView': {
          debugLog('scrollIntoView port started', '')
          document.getElementById(arg.data.id).scrollIntoView(true)
          break
        }
        case 'validateDeadline': {
          debugLog('validatingDate port started', '')

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
          debugLog('hideFooter port started', '')
          document.getElementById('guest-footer').className += ' guest__footer'
          break
        }
        case 'subscribeToNewCommunity': {
          debugLog('subscribeToNewCommunity port started', arg)
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
          break
        }
        case 'subscribeToTransfer': {
          debugLog('subscribeToTransfer port started', arg)

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

          break
        }
        case 'subscribeToUnreadCount': {
          debugLog('unreadCountSubscription port started', arg.data.subscription)
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
          break
        }
        case 'copyToClipboard': {
          debugLog('copyToClipboard port started', '')
          document.querySelector('#' + arg.data.id).select()
          document.execCommand('copy')
          const response = {
            address: arg.responseAddress,
            addressData: arg.responseData
          }
          app.ports.javascriptInPort.send(response)
          break
        }
        default: {
          debugLog('No treatment found for Elm port ', arg.data.name)
        }
      }
    }

    // We need to run this again because Elm clears everything from `body`
    setupIframe(false)
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
