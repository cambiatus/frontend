/* global HTMLElement, CustomEvent */

export default () => (
  class KeyListener extends HTMLElement {
    static get observedAttributes () { return ['keydown-stop-propagation', 'keydown-prevent-default', 'accepted-keys'] }

    constructor () {
      super()

      this._keydownListener = (e) => {
        if (this._acceptedKeys && this._acceptedKeys.includes(e.key.toLowerCase())) {
          if (this._keydownPreventDefault) {
            e.preventDefault()
          }

          if (this._keydownStopPropagation) {
            e.stopPropagation()
          }
        }

        this.dispatchEvent(new CustomEvent('listener-keydown', { detail: { key: e.key } }))
      }
    }

    attributeChangedCallback (name, oldValue, newValue) {
      switch (name) {
        case 'keydown-stop-propagation':
          this._keydownStopPropagation = newValue === 'true'
          break
        case 'keydown-prevent-default':
          this._keydownPreventDefault = newValue === 'true'
          break
        case 'accepted-keys':
          this._acceptedKeys = newValue.split(',')
          break
        default:
          break
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
