import * as AbsintheSocket from '@absinthe/socket'
import * as paypalJs from '@paypal/paypal-js'
import * as Sentry from '@sentry/browser'
import Eos from 'eosjs'
import ecc from 'eosjs-ecc'
import * as pdfjsLib from 'pdfjs-dist/es5/build/pdf'
import pdfMake from 'pdfmake/build/pdfmake'
import Quill from 'quill'
import QuillDelta from 'quill-delta'
import sjcl from 'sjcl'
import { Elm } from './elm/Main.elm'
import configuration from './scripts/config.js'
import mnemonic from './scripts/mnemonic.js'
// import registerServiceWorker from './scripts/registerServiceWorker'
import pdfDefinition from './scripts/pdfDefinition'
import './styles/main.css'
import pdfFonts from './vfs_fonts'
import 'focus-visible'

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
const graphqlSecret = process.env.GRAPHQL_SECRET || ''
const useSubdomain = process.env.USE_SUBDOMAIN === undefined ? true : process.env.USE_SUBDOMAIN !== 'false'
const config = configuration[env]

// If you're updating `pdfjs-dist`, make sure to
// `cp ./node_modules/pdfjs-dist/es5/build/pdf.worker.min.js ./public`
pdfjsLib.GlobalWorkerOptions.workerSrc = '/pdf.worker.min.js'

// =========================================
// Custom elements
// =========================================
/* global HTMLElement, CustomEvent */

window.customElements.define('paypal-buttons',
  class PaypalButtons extends HTMLElement {
    static get observedAttributes () { return ['elm-currency'] }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'elm-currency') {
        this.removePaypal()
        this.setupPaypal()
      }
    }

    constructor () {
      super()

      const shadow = this.attachShadow({ mode: 'open' })
      this._paypalContainer = document.createElement('div')
      shadow.appendChild(this._paypalContainer)
    }

    disconnectedCallback () {
      this.removePaypal()
    }

    removePaypal () {
      for (const child of document.head.childNodes) {
        if (child.nodeName.toLowerCase() === 'script' && child.src) {
          if (child.src.startsWith('https://www.paypal.com/sdk/js?')) {
            document.head.removeChild(child)
          }
        }
      }
    }

    setupPaypal () {
      const handleError = (err) => {
        if (err.message !== 'Elm got an error creating contribution') {
          this.dispatchEvent(new CustomEvent('paypal-error', { error: err }))
        }
      }

      paypalJs.loadScript({
        'client-id': config.paypal.clientId,
        currency: this.getAttribute('elm-currency') || 'USD'
      })
        .then((paypal) => {
          paypal.Buttons({
            style: {
              shape: 'pill'
            },

            createOrder: async (data, actions) => {
              const valueAttribute = this.getAttribute('elm-value')
              if (valueAttribute === '') {
                return handleError(new Error('Amount could not be parsed as a float by Elm'))
              }

              app.ports.requestPaypalInfoFromJs.send(this.id)
              const paypalInfo = await usePortAsPromise(app.ports.paypalInfo, (paypalInfo) => {
                if (paypalInfo.targetId !== this.id) {
                  return { unsubscribeFromPort: false }
                }

                return paypalInfo
              })

              if (paypalInfo.error) {
                return handleError(new Error('Elm got an error creating contribution'))
              }

              return actions.order.create({
                purchase_units: [{
                  invoice_id: paypalInfo.invoiceId,
                  amount: {
                    value: paypalInfo.amount,
                    currency_code: paypalInfo.currency
                  }
                }],
                application_context: {
                  brand_name: paypalInfo.communityName,
                  shipping_preference: 'NO_SHIPPING'
                }
              })
            },

            onApprove: (data, actions) => {
              return actions.order.capture().then(() => {
                this.dispatchEvent(new CustomEvent('paypal-approve', {}))
              })
            },

            onCancel: () => {
              this.dispatchEvent(new CustomEvent('paypal-cancel', {}))
            },

            onError: handleError
          }).render(this._paypalContainer)
        })
        .catch((err) => {
          this.dispatchEvent(new CustomEvent('paypal-load-error', { error: err }))
        })
    }
  }
)

window.customElements.define('masked-input-helper',
  class MaskedInputHelper extends HTMLElement {
    static get observedAttributes () { return ['decimal-separator'] }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'decimal-separator') {
        this._decimalSeparator = newValue
      }
    }

    connectedCallback () {
      this.className = 'hidden'

      const targetElement = document.getElementById(this.getAttribute('target-id'))
      if (!targetElement) throw new Error('Couldn\'t find target element for masked-input-helper')

      const maskType = this.getAttribute('mask-type')
      let previousSelectionStart = targetElement.selectionStart || 0
      let previousSelectionEnd = targetElement.selectionEnd || 0
      let previousValue = targetElement.value || ''

      this.inputListener = (e) => {
        window.setTimeout(() => {
          const newSelectionStart = targetElement.selectionStart || 0
          const isAtEnd = maskType === 'string' ? previousSelectionStart === previousValue.length : false
          const isDeletingNumber = e.data === null && maskType === 'number'
          const isChangingMask = Math.abs(previousSelectionStart - newSelectionStart) > 1

          if (maskType === 'number') {
            if (e.data === this._decimalSeparator) {
              if (targetElement.value.indexOf(this._decimalSeparator) === -1) return

              const newSelectionStart = targetElement.value.indexOf(this._decimalSeparator) + 2
              previousSelectionStart = newSelectionStart
              previousSelectionEnd = newSelectionStart
              previousValue = targetElement.value
              targetElement.setSelectionRange(newSelectionStart, newSelectionStart)
              return
            }

            if (previousSelectionStart === previousValue.indexOf(this._decimalSeparator) + 1 && !isDeletingNumber) {
              previousValue = targetElement.value
              previousSelectionStart = targetElement.value.indexOf(this._decimalSeparator)
              previousSelectionEnd = previousSelectionStart

              targetElement.setSelectionRange(previousSelectionStart, previousSelectionStart)
              return
            }
          }

          if (Math.abs(previousSelectionStart - previousSelectionEnd) > 0) {
            const newSelectionIndex = Math.max(previousSelectionStart, previousSelectionEnd) + 1
            targetElement.setSelectionRange(newSelectionIndex, newSelectionIndex)

            previousValue = targetElement.value
            previousSelectionStart = newSelectionIndex
            previousSelectionEnd = newSelectionIndex
            return
          }

          if ((isDeletingNumber || isChangingMask) && !isAtEnd) {
            const sumFactor = e.data !== null && targetElement.value.length >= previousValue.length
              ? +1
              : maskType === 'number' ? 0 : -1
            const newIndex = this.firstIndexAfter(targetElement.value, previousSelectionStart, e.data) + sumFactor
            targetElement.setSelectionRange(newIndex, newIndex)

            previousValue = targetElement.value
            previousSelectionStart = newIndex
            previousSelectionEnd = newIndex
            return
          }

          previousSelectionStart = newSelectionStart
          previousValue = targetElement.value
        }, 0)
      }

      this.keyDownListener = (e) => {
        let originalSelectionStart = targetElement.selectionStart
        if (Math.abs(targetElement.selectionStart - previousSelectionStart) < 2 || e.ctrlKey) {
          window.setTimeout(() => {
            if (Math.abs(targetElement.selectionStart - originalSelectionStart) < 2 || e.ctrlKey) {
              previousSelectionStart = targetElement.selectionStart
              previousSelectionEnd = targetElement.selectionEnd
            }
          }, 0)
        }
      }

      this.clickListener = () => {
        previousSelectionStart = targetElement.selectionStart
        previousSelectionEnd = targetElement.selectionEnd
      }

      targetElement.addEventListener('input', this.inputListener)
      targetElement.addEventListener('keydown', this.keyDownListener)
      targetElement.addEventListener('click', this.clickListener)
    }

    disconnectedCallback () {
      const targetElement = document.getElementById(this.getAttribute('target-id'))

      if (!targetElement) return

      targetElement.removeEventListener('input', this.inputListener)
      targetElement.removeEventListener('keydown', this.keyDownListener)
      targetElement.removeEventListener('click', this.clickListener)
    }

    firstIndexAfter (stringValue, baseIndex, element) {
      for (let index = 0; index < stringValue.length; index++) {
        if (stringValue[index] === element && index >= baseIndex) {
          return index
        }
      }

      return baseIndex
    }
  }
)

window.customElements.define('focus-trap',
  class FocusTrap extends HTMLElement {
    constructor () {
      super()

      this._previouslyFocusedElement = document.activeElement

      this._keydownListener = (e) => {
        const isTab = e.key === 'Tab' || e.keyCode === 9

        if (!isTab) {
          return
        }

        const focusables = this.focusables(this)
        const firstFocusable = focusables[0]
        const lastFocusable = focusables[focusables.length - 1]

        if (e.shiftKey && document.activeElement === firstFocusable) {
          e.preventDefault()
          lastFocusable.focus()
        } else if (!e.shiftKey && document.activeElement === lastFocusable) {
          e.preventDefault()
          firstFocusable.focus()
        }
      }
    }

    connectedCallback () {
      let elementToFocus = this.focusables(this)[0]
      const firstFocusableContainers = this.querySelectorAll(this.getAttribute('first-focus-container'))
      for (const container of firstFocusableContainers) {
        const focusables = this.focusables(container)
        if (focusables.length > 0) {
          elementToFocus = focusables[0]
          break
        }
      }

      if (elementToFocus) {
        elementToFocus.focus()
      }

      document.addEventListener('keydown', this._keydownListener)
    }

    disconnectedCallback () {
      this._previouslyFocusedElement.focus()
      document.removeEventListener('keydown', this._keydownListener)
    }

    focusables (parent) {
      const tabbableElements = [
        'a[href]',
        'button',
        'textarea',
        'input[type="text"]',
        'input[type="radio"]',
        'input[type="checkbox"]',
        'select',
        '[contenteditable="true"]'
      ].map((selector) => `${selector}:not([disabled]):not([tabindex="-1"])`)

      return parent.querySelectorAll(tabbableElements.join(', '))
    }
  }
)

window.customElements.define('key-listener',
  class KeyListener extends HTMLElement {
    static get observedAttributes () { return ['keydown-stop-propagation'] }

    constructor () {
      super()

      this._keydownListener = (e) => {
        if (this._keydownStopPropagation) {
          e.stopPropagation()
        }
        this.dispatchEvent(new CustomEvent('listener-keydown', { detail: { key: e.key } }))
      }
    }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'keydown-stop-propagation') {
        this._keydownStopPropagation = newValue === 'true'
      }
    }

    connectedCallback () {
      document.addEventListener('keydown', this._keydownListener)
    }

    disconnectedCallback () {
      document.removeEventListener('keydown', this._keydownListener)
    }
  }
)

window.customElements.define('markdown-editor',
  class MarkdownEditor extends HTMLElement {
    static get observedAttributes () { return ['elm-edit-text', 'elm-remove-text', 'elm-disabled', 'elm-has-error'] }

    constructor () {
      super()

      this._quillContainer = document.createElement('div')
      this._parentContainer = document.createElement('div')
      this._parentContainer.className = 'placeholder-gray-400 input-border'

      this._parentContainer.appendChild(this._quillContainer)
      this._quill = new Quill(this._quillContainer,
        {
          modules: {
            toolbar: [
              [{ 'header': 1 }, { 'header': 2 }],
              ['bold', 'italic', 'strike'],
              ['link'],
              [{ 'list': 'ordered' }, { 'list': 'bullet' }]
            ]
          },
          formats: ['header', 'bold', 'code', 'italic', 'link', 'strike', 'list'],
          placeholder: this.getAttribute('elm-placeholder'),
          theme: 'snow'
        }
      )

      app.ports.setMarkdown.subscribe((data) => { this.setMarkdownListener(data) })

      this._quill.on('text-change', (delta, oldDelta, source) => {
        const contents = this._quill.getContents()
        this.dispatchEvent(new CustomEvent('text-change', { detail: contents }))
      })

      const toolbar = this._quill.getModule('toolbar')
      toolbar.addHandler('link', () => { this.linkHandler() })
    }

    connectedCallback () {
      // If we dont include the timeout, we get some annoying bugs in
      // development where the text is cleared, and hot reloading bugs out and
      // crashes the app
      window.setTimeout(() => {
        this.dispatchEvent(new CustomEvent('component-loaded', {}))
      }, 0)
      this.appendChild(this._parentContainer)

      // Remove default click handler and add our custom one
      const oldEditButton = this.querySelector('.ql-tooltip a.ql-action')
      const editButton = oldEditButton.cloneNode(true)
      oldEditButton.parentNode.replaceChild(editButton, oldEditButton)
      editButton.addEventListener('click', (e) => {
        this.querySelector('.ql-tooltip').classList.add('ql-hidden')
        this.linkHandler()
      })

      const editor = this.querySelector('.ql-editor')
      if (editor) {
        editor.addEventListener('focus', () => { this.dispatchEvent(new CustomEvent('focus', {})) })
        editor.addEventListener('blur', () => { this.dispatchEvent(new CustomEvent('blur', {})) })
      }

      this.setTooltipTexts()
      this.setDisabled()
    }

    attributeChangedCallback (name) {
      if (name === 'elm-disabled') {
        this.setDisabled()
      } else if (name === 'elm-has-error') {
        this.toggleHasError()
      } else {
        this.setTooltipTexts()
      }
    }

    setTooltipTexts () {
      const actionButton = this.querySelector('.ql-tooltip a.ql-action')
      if (actionButton) {
        actionButton.setAttribute('data-edit-text', this.getAttribute('elm-edit-text'))
      }

      const removeButton = this.querySelector('.ql-tooltip a.ql-remove')
      if (removeButton) {
        removeButton.setAttribute('data-remove-text', this.getAttribute('elm-remove-text'))
      }
    }

    toggleHasError () {
      const hasError = this.getAttribute('elm-has-error') === 'true'
      if (hasError) {
        this._parentContainer.classList.add('with-error')
      } else {
        this._parentContainer.classList.remove('with-error')
      }
    }

    setDisabled () {
      const isDisabled = this.getAttribute('elm-disabled') === 'true'

      if (isDisabled) {
        this._quill.disable()
      } else {
        this._quill.enable()
      }
    }

    linkHandler () {
      if (!this._quill.isEnabled()) {
        return
      }

      let range = this._quill.getSelection(true)
      const isLink = this._quill.getFormat(range).link !== undefined
      if (range.length === 0 && isLink) {
        range = this.getFormattedRange(range.index)
      }
      const text = this._quill.getText(range)
      const currentFormat = this._quill.getFormat(range)
      this.dispatchEvent(new CustomEvent('clicked-include-link',
        {
          detail: {
            label: text,
            url: currentFormat.link || ''
          }
        }
      ))

      usePortAsPromise(app.ports.markdownLink, (link) => {
        if (link.id === this.id) {
          this._quill.updateContents(new QuillDelta()
            .retain(range.index)
            .delete(range.length)
            .insert(link.label, { ...currentFormat, link: link.url })
          )

          this._quill.setSelection(range.index + link.label.length, 0, 'silent')
        } else {
          return { unsubscribeFromPort: false }
        }
      })
    }

    /** Gets the range from the formatting that the `index` position is affected
    by. Useful when the user has their caret in the middle of a link, but isn't
    actually selecting it (selection length is 0)
    */
    getFormattedRange (index) {
      let currentIndex = 0
      for (const content of this._quill.getContents().ops) {
        const initialIndex = currentIndex
        const finalIndex = initialIndex + content.insert.length - 1
        currentIndex = finalIndex + 1
        if (initialIndex <= index && index <= finalIndex) {
          return { index: initialIndex, length: finalIndex - initialIndex + 1 }
        }
      }
      return { index: 0, length: 0 }
    }

    setMarkdownListener (data) {
      if (data.id === this.id) {
        this._quill.setContents(data.content)
      }
    }
  }
)

window.customElements.define('infinite-list',
  class InfiniteList extends HTMLElement {
    static get observedAttributes () { return ['elm-distance-to-request', 'elm-element-to-track'] }

    connectedCallback () {
      this.listenToScroll()

      window.addEventListener('resize', () => { this.listenToScroll() })
    }

    attributeChangedCallback () {
      this.listenToScroll()
    }

    listenToScroll () {
      if (this._scrollInterval) {
        clearInterval(this._scrollInterval)
      }

      let scrolling = false
      const isHidden = this.getBoundingClientRect().width === 0 && this.getBoundingClientRect().height === 0
      if (isHidden) {
        return
      }

      const whatToTrack = this.getAttribute('elm-element-to-track')
      if (!whatToTrack) {
        return
      }

      let elementToTrack
      let elementToListen
      if (whatToTrack === 'track-window') {
        elementToTrack = this
        elementToListen = window
      } else if (whatToTrack === 'track-self') {
        elementToTrack = this
        elementToListen = this
      } else {
        elementToTrack = document.querySelector(whatToTrack)
        elementToListen = elementToTrack
      }

      if (!elementToTrack) {
        return
      }

      elementToListen.addEventListener('scroll', () => { scrolling = true })
      const distanceToRequest = this.getAttribute('elm-distance-to-request') || 0
      this._scrollInterval = setInterval(() => {
        if (scrolling) {
          scrolling = false
          if (elementToTrack.scrollTop >= elementToTrack.scrollHeight - distanceToRequest - elementToTrack.getBoundingClientRect().height) {
            this.dispatchEvent(new CustomEvent('requested-items', {}))
          }
        }
      }, 300)
    }
  }
)

window.customElements.define('date-formatter',
  class DateFormatter extends HTMLElement {
    static get observedAttributes () { return ['elm-locale', 'elm-date'] }

    constructor () {
      super()

      const shadow = this.attachShadow({ mode: 'open' })
      const textContainer = document.createElement('span')
      this._textContainer = textContainer
      shadow.appendChild(textContainer)
    }

    connectedCallback () {
      this.setDateText()
    }

    attributeChangedCallback () {
      this.setDateText()
    }

    setDateText () {
      const locale = this.getAttribute('elm-locale')
      const date = new Date(parseInt(this.getAttribute('elm-date')))
      const dayString = date.toLocaleDateString(locale, { day: 'numeric' })
      const monthString = date.toLocaleDateString(locale, { month: 'short' })
        .replace(/[.]/g, '')
      const yearString = date.toLocaleDateString(locale, { year: 'numeric' })

      const translationString = this.getAttribute('elm-translation') === null ? '{{date}}' : this.getAttribute('elm-translation')
      const translatedString = translationString.replace(/{{date}}/, `${dayString} ${monthString} ${yearString}`)
      this._textContainer.textContent = translatedString
    }
  }
)

window.customElements.define('pdf-viewer',
  class PdfViewer extends HTMLElement {
    connectedCallback () {
      if (this.hasChildNodes()) return

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
          addBreadcrumb({
            type: 'error',
            category: 'pdf-viewer',
            message: 'Got an error when trying to display a PDF',
            data: { error: e },
            localData: {},
            level: 'warning'
          })
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

      window.addEventListener('resize', this.setPosition, { passive: true })
    }

    connectedCallback () {
      this.className = `${this.getAttribute('elm-class')} ${this._defaultClasses}`
      const bgClass = this.className.match(/bg-[a-z]*/)[0]

      const point = document.createElement('div')
      const pointElement = document.createElement('div')
      pointElement.className = `w-8 h-8 ${bgClass} transform -rotate-45 rounded-sm`

      const relativeSelector = this.getAttribute('elm-relative-selector')
      this._relativeElement = relativeSelector ? document.querySelector(relativeSelector) : null

      const scrollSelector = this.getAttribute('elm-scroll-selector')
      this._scrollSelector = scrollSelector ? document.querySelector(scrollSelector) : null

      if (this._scrollSelector !== null) {
        this._scrollSelector.addEventListener('scroll', this.setPosition, { passive: true })
      } else {
        window.addEventListener('scroll', this.setPosition)
      }

      point.appendChild(pointElement)

      point.className = this._defaultPointClasses
      this.appendChild(point)

      this._point = point

      this._sibling = this.previousSibling || this.nextSibling

      this.setPosition()
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.setPosition)
      if (this._scrollSelector !== null) {
        this._scrollSelector.removeEventListener('scroll', this.setPosition)
      } else {
        window.removeEventListener('scroll', this.setPosition)
      }
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
    constructor () {
      super()
      this._classesHandledByOthers = []
    }
    connectedCallback () {
      this._preventScrollingClasses = this.getAttribute('elm-prevent-scroll-class').split(' ')
      this._preventScrollingClasses.forEach((class_) => {
        if (document.body.classList.contains(class_)) {
          this._classesHandledByOthers.push(class_)
          return
        }

        document.body.classList.add(class_)
      })
    }

    disconnectedCallback () {
      this._preventScrollingClasses.forEach((class_) => {
        if (this._classesHandledByOthers.includes(class_) || !document.body.classList.contains(class_)) {
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
  return !!navigator.clipboard && !!navigator.clipboard.readText
}

/** Assumes we already have clipboard permissions */
async function readClipboardWithPermission () {
  try {
    const clipboardContent = await navigator.clipboard.readText()
    return { clipboardContent }
  } catch (err) {
    logEvent({
      user: null,
      message: 'Error when reading clipboard',
      tags: { 'cambiatus.kind': 'clipboard' },
      contexts: [{ name: 'Error details', extras: { error: err } }],
      transaction: 'readClipboardWithPermission',
      level: 'error'
    })

    return { error: err.message }
  }
}

function flags () {
  const user = JSON.parse(getItem(USER_KEY))
  const accountName = (user && user.accountName) || null

  if (accountName !== null) {
    Sentry.configureScope((scope) => { scope.setUser({ username: accountName }) })
  }

  return {
    graphqlSecret: graphqlSecret,
    endpoints: config.endpoints,
    language: getUserLanguage(),
    accountName: accountName,
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

// Register Service Worker After App
// registerServiceWorker()

/**
 * Subscribe to a port to consume it only once and unsubcribe from it.
 * It's good practice to have an id to check if that port is actually supposed
 * to communicate with the component that requested it. In case it's not, the
 * `handler` should return `{ unsubscribeFromPort: false }` (that way the
 * component doesn't unsubscribe from the port)
 * @param {*} port an elm port
 * @param {*} handler a function to act as the port's subscription
 * @returns a promise that resolves with the result of `handler`
 */
const usePortAsPromise = (port, handler) => {
  return new Promise((resolve, reject) => {
    const internalHandler = (...args) => {
      const result = handler(...args)
      if (result && (result.unsubcribeFromPort === undefined || result.unsubscribeFromPort !== false)) {
        port.unsubscribe(internalHandler)
        resolve(result)
      }
    }

    port.subscribe(internalHandler)
  })
}

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

app.ports.addPlausibleScriptPort.subscribe(({ domain, src }) => {
  const plausibleScript = document.createElement('script')
  plausibleScript.setAttribute('src', src)
  plausibleScript.dataset.domain = domain

  document.head.appendChild(plausibleScript)
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
  Sentry.configureScope((scope) => { scope.setUser(null) })
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
    case 'login': {
      const passphrase = arg.data.passphrase
      const privateKey = ecc.seedPrivate(mnemonic.toSeedHex(passphrase))

      if (!ecc.isValidPrivate(privateKey)) {
        return { error: 'error.invalidKey' }
      } else {
        try {
          const publicKey = ecc.privateToPublic(privateKey)
          const accounts = await eos.getKeyAccounts(publicKey)
          addBreadcrumb({
            type: 'debug',
            category: 'login',
            message: 'Got accounts from public key',
            data: { numberOfAccounts: accounts.account_names.length },
            localData: { accounts },
            level: 'debug'
          })

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
            Sentry.configureScope((scope) => { scope.setUser({ username: accountName }) })
            addBreadcrumb({
              type: 'debug',
              category: 'login',
              message: 'Saved credentials to EOS',
              data: {},
              localData: { accountName },
              level: 'debug'
            })

            return { accountName, privateKey }
          }
        } catch (err) {
          logEvent({
            user: null,
            message: 'Login port error',
            tags: { 'cambiatus.kind': 'auth' },
            contexts: [{ name: 'Error details', extras: { error: err } }],
            transaction: 'login',
            level: 'error'
          })

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
      document.querySelector('#' + arg.data.id).select()
      document.execCommand('copy')

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
    default: {
      return { error: `No treatment found for Elm port ${arg.data.name}` }
    }
  }
}
