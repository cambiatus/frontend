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
import * as pdfjsLib from 'pdfjs-dist/es5/build/pdf'

// If you're updating `pdfjs-dist`, make sure to
// `cp ./node_modules/pdfjs-dist/es5/build/pdf.worker.min.js ./public`
pdfjsLib.GlobalWorkerOptions.workerSrc = '/pdf.worker.min.js'

// =========================================
// Custom elements
// =========================================
/* global HTMLElement */

window.customElements.define('pdf-viewer',
  class PdfViewer extends HTMLElement {
    connectedCallback () {
      const url = this.getAttribute('elm-url')
      const childClass = this.getAttribute('elm-child-class')

      const loadingImg = document.createElement('img')
      loadingImg.src = '/images/loading.svg'
      this.appendChild(loadingImg)

      let setContent = (node) => {
        this.removeChild(loadingImg)
        this.appendChild(node)
      }

      if (this.getAttribute('elm-loading-title') && this.getAttribute('elm-loading-subtitle')) {
        loadingImg.className = 'h-16 mt-8'

        const loadingTitle = document.createElement('p')
        loadingTitle.className = 'font-bold text-2xl'
        loadingTitle.textContent = this.getAttribute('elm-loading-title')
        this.appendChild(loadingTitle)

        const loadingSubtitle = document.createElement('p')
        loadingSubtitle.className = 'text-sm'
        loadingSubtitle.textContent = this.getAttribute('elm-loading-subtitle')
        this.appendChild(loadingSubtitle)

        setContent = (node) => {
          this.removeChild(loadingImg)
          this.removeChild(loadingTitle)
          this.removeChild(loadingSubtitle)
          this.appendChild(node)
        }
      } else {
        loadingImg.className = 'p-4'
      }

      const notFoundTimeout = window.setTimeout(() => {
        const notFoundImg = document.createElement('img')
        notFoundImg.src = '/icons/pdf.svg'
        setContent(notFoundImg)
        const bgColor = 'bg-purple-500'
        this.classList.add(bgColor)

        setContent = (node) => {
          this.removeChild(notFoundImg)
          if (this.classList.contains(bgColor)) {
            this.classList.remove(bgColor)
          }
          this.appendChild(node)
        }
      }, 1000 * 3)

      pdfjsLib.getDocument(url).promise.then((pdf) => {
        pdf.getPage(1).then((page) => {
          const canvas = document.createElement('canvas')
          canvas.className = childClass

          const width = this.clientWidth
          const height = this.clientHeight
          const unscaledViewport = page.getViewport({ scale: 1 })
          const scale = Math.min((height / unscaledViewport.height), (width / unscaledViewport.width))

          const viewport = page.getViewport({ scale })
          const canvasContext = canvas.getContext('2d')
          canvas.width = viewport.width
          canvas.height = viewport.height

          const renderContext = { canvasContext, viewport }

          const renderTask = page.render(renderContext)
          renderTask.promise.then(() => {
            window.clearTimeout(notFoundTimeout)
            setContent(canvas)
          })
        })
      }).catch((e) => {
        const invalidPDFError = 'Invalid PDF structure.'
        if (e.message === invalidPDFError) {
          const img = document.createElement('img')
          img.src = url
          img.className = childClass
          window.clearTimeout(notFoundTimeout)
          setContent(img)
        } else {
          debugLog('pdf-viewer error', e)
        }
      })
    }
  }
)

window.customElements.define('dialog-bubble',
  class DialogBubble extends HTMLElement {
    constructor () {
      super()

      this._defaultClasses = 'absolute transform cursor-auto z-50 p-6 bg-white flex rounded shadow-2xl'
      this._defaultPointClasses = 'absolute transform -z-10'

      window.addEventListener('resize', () => { this.setPosition() }, { passive: true })
    }

    connectedCallback () {
      this.className = `${this.getAttribute('elm-class')} ${this._defaultClasses}`
      const point = document.createElement('div')
      const pointElement = document.createElement('div')
      pointElement.className = 'w-8 h-8 bg-white transform -rotate-45 rounded-sm'

      const relativeSelector = this.getAttribute('elm-relative-selector')
      this._relativeElement = relativeSelector ? document.querySelector(relativeSelector) : null

      const scrollSelector = this.getAttribute('elm-scroll-selector')
      this._scrollSelector = scrollSelector ? document.querySelector(scrollSelector) : null

      if (this._scrollSelector !== null) {
        this._scrollSelector.addEventListener('scroll', () => { this.setPosition() }, { passive: true })
      } else {
        window.addEventListener('scroll', () => { this.setPosition() })
      }

      point.appendChild(pointElement)

      point.className = this._defaultPointClasses
      this.appendChild(point)

      this._point = point

      this._sibling = this.previousSibling || this.nextSibling

      this.setPosition()
    }

    setPosition () {
      if (!this._sibling) return

      const relativeTop = this._relativeElement ? this._relativeElement.getBoundingClientRect().top : -window.scrollY
      const relativeLeft = this._relativeElement ? this._relativeElement.getBoundingClientRect().left : -window.scrollX

      const siblingRect = this._sibling.getBoundingClientRect()
      const siblingTop = siblingRect.top
      const siblingLeft = siblingRect.left
      const targetTop = siblingRect.top - relativeTop
      const targetLeft = siblingRect.left - relativeLeft
      const thisRect = this.getBoundingClientRect()

      let pointPositionClasses = ''
      let top = siblingTop
      let left = siblingLeft
      if (siblingLeft <= thisRect.width / 2) {
        // Go to the right
        top = targetTop - thisRect.height / 2 + siblingRect.height / 2
        left = targetLeft + siblingRect.width
        pointPositionClasses = '-left-1 top-1/2 -translate-y-1/2'
      } else if (siblingLeft + siblingRect.width + thisRect.width / 2 >= window.innerWidth) {
        // Go to the left
        top = targetTop - thisRect.height / 2 + siblingRect.height / 2
        left = targetLeft - thisRect.width
        pointPositionClasses = '-right-1 top-1/2 -translate-y-1/2'
      } else if (siblingTop <= thisRect.height) {
        // Go down
        top = targetTop + siblingRect.height
        left = targetLeft + siblingRect.width / 2 - thisRect.width / 2
        pointPositionClasses = '-top-1 right-1/2 translate-x-1/2'
      } else {
        // Go up
        top = targetTop - thisRect.height
        left = targetLeft + siblingRect.width / 2 - thisRect.width / 2
        pointPositionClasses = '-bottom-1 right-1/2 translate-x-1/2'
      }

      this.style.top = `${top}px`
      this.style.left = `${left}px`

      this._point.className = `${this._defaultPointClasses} ${pointPositionClasses}`
    }
  }
)

window.customElements.define('bg-no-scroll',
  class BgNoScroll extends HTMLElement {
    connectedCallback () {
      this._preventScrollingClasses = this.getAttribute('elm-prevent-scroll-class').split(' ')
      this._preventScrollingClasses.forEach((class_) => {
        if (document.body.classList.contains(class_)) {
          return
        }

        document.body.classList.add(class_)
      })
    }

    disconnectedCallback () {
      this._preventScrollingClasses.forEach((class_) => {
        if (!document.body.classList.contains(class_)) {
          return
        }

        document.body.classList.remove(class_)
      })
    }
  }
)

// =========================================
// App startup
// =========================================

let eos = null
const USER_KEY = 'bespiral.user'
const LANGUAGE_KEY = 'bespiral.language'
const PUSH_PREF = 'bespiral.push.pref'
const AUTH_TOKEN = 'bespiral.auth_token'
const RECENT_SEARCHES = 'bespiral.recent_search'
const SELECTED_COMMUNITY_KEY = 'bespiral.selected_community'
const env = process.env.NODE_ENV || 'development'
const graphqlSecret = process.env.GRAPHQL_SECRET || ''
const useSubdomain = process.env.USE_SUBDOMAIN === undefined ? true : process.env.USE_SUBDOMAIN !== 'false'
const config = configuration[env]

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
  const result = document.cookie.match('(^|[^;]+)\\s*' + cookieKey(key) + '\\s*=\\s*([^;]+)')
  return result ? result.pop() : null
}

const removeItem = (key) => {
  document.cookie = `${cookieKey(key)}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; ${cookieDomain()}; path=/; SameSite=Strict; Secure`
  window.localStorage.removeItem(key)
}

const setItem = (key, value) => {
  // This is the maximum possible expiration date for some browsers, because
  // they use 32 bits to represent this field (maxExpirationDate === 2^31 - 1).
  // This is equivalent to the date 2038-01-19 04:14:07
  const maxExpirationDate = 2147483647
  document.cookie = `${cookieKey(key)}=${value}; expires=${new Date(maxExpirationDate * 1000).toUTCString()}; ${cookieDomain()}; path=/; SameSite=Strict; Secure`
}

const storedKeys = [USER_KEY, LANGUAGE_KEY, PUSH_PREF, AUTH_TOKEN, RECENT_SEARCHES, SELECTED_COMMUNITY_KEY]

storedKeys.forEach((key) => {
  const localStorageValue = window.localStorage.getItem(key)
  if (localStorageValue !== null) {
    setItem(key, localStorageValue)
    window.localStorage.removeItem(key)
  }
})

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
    useSubdomain: useSubdomain,
    selectedCommunity: getItem(SELECTED_COMMUNITY_KEY)
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

app.ports.storeSelectedCommunitySymbol.subscribe(symbol => {
  setItem(SELECTED_COMMUNITY_KEY, symbol)
  debugLog(`stored community`, symbol)
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
