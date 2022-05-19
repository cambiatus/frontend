/* global HTMLElement, CustomEvent */

export default () => (
  class PointerListener extends HTMLElement {
    connectedCallback () {
      this.listeners = {
        'pointermove': (e) => {
          this.dispatchEvent(new CustomEvent('document-pointermove', {
            detail: {
              clientX: e.clientX,
              clientY: e.clientY
            }
          }))
        },
        'pointerup': () => {
          this.dispatchEvent(new CustomEvent('document-pointerup'))
        }
      }

      Object.keys(this.listeners).forEach((key) => {
        document.addEventListener(key, this.listeners[key])
      })
    }

    disconnectedCallback () {
      Object.keys(this.listeners).forEach((key) => {
        document.removeEventListener(key, this.listeners[key])
      })
      this.listeners = undefined
    }
  }
)
