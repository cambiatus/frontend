/* global HTMLElement */

export default () => (
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
      const initialFocusId = this.getAttribute('initial-focus-id')
      if (initialFocusId) {
        elementToFocus = this.querySelector(`#${initialFocusId}`)
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
