/* global HTMLElement, CustomEvent */

export default () => (
  class PointerListener extends HTMLElement {
    connectedCallback () {
      this.previousX = null
      this.previousY = null

      this.listeners = {
        'pointermove': (e) => {
          if (this.previousX == null || this.previousY == null) {
            this.previousX = e.clientX
            this.previousY = e.clientY
            return
          }

          this.dispatchEvent(new CustomEvent('document-pointermove', {
            detail: {
              clientX: e.clientX,
              clientY: e.clientY,
              previousX: this.previousX,
              previousY: this.previousY
            }
          }))

          this.previousX = e.clientX
          this.previousY = e.clientY
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
