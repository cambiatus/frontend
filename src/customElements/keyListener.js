/* global HTMLElement, CustomEvent */

export default () => (
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
