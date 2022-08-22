/* global HTMLElement, CustomEvent */

export default () => (
  class PointerListener extends HTMLElement {
    connectedCallback () {
      this.previousX = null
      this.previousY = null

      this.documentListeners = {
        'dragover': (e) => {
          if (this.previousX == null || this.previousY == null) {
            this.previousX = e.clientX
            this.previousY = e.clientY
            return
          }

          if (this.previousX == e.clientX && this.previousY == e.clientY) {
            return
          }

          this.dispatchEvent(new CustomEvent('document-dragover', {
            detail: {
              clientX: e.clientX,
              clientY: e.clientY,
              previousX: this.previousX,
              previousY: this.previousY
            }
          }))

          this.previousX = e.clientX
          this.previousY = e.clientY
        }
      }

      this.elementListeners = {
        'dragstart': (e) => {
          this.dispatchEvent(new CustomEvent('element-dragstart'))

          const dragImageElement = document.createElement('span')

          dragImageElement.classList.add('sr-only')
          dragImageElement.classList.add('pointer-events-none')
          dragImageElement.setAttribute('aria-hidden', 'true')

          e.dataTransfer.setDragImage(dragImageElement, 0, 0)
        },
        'dragend': () => {
          this.dispatchEvent(new CustomEvent('element-dragend'))
        }
      }


      Object.keys(this.documentListeners).forEach((key) => {
        document.addEventListener(key, this.documentListeners[key])
      })

      Object.keys(this.elementListeners).forEach((key) => {
        this.addEventListener(key, this.elementListeners[key])
      })
    }

    disconnectedCallback () {
      Object.keys(this.elementListeners).forEach((key) => {
        this.removeEventListener(key, this.elementListeners[key])
      })

      Object.keys(this.documentListeners).forEach((key) => {
        document.removeEventListener(key, this.documentListeners[key])
      })
      this.documentListeners = undefined
    }
  }
)
