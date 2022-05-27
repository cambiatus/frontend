/* global HTMLElement, CustomEvent, Image, File */

export default () => (
  class ImageCropper extends HTMLElement {
    static get observedAttributes () {
      return ['elm-generate-new-cropped-image']
    }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'elm-generate-new-cropped-image' && newValue) {
        this.crop()
      }
    }

    connectedCallback () {
      this.documentResizeListener = () => {
        this.dispatchEvent(new CustomEvent('document-resize'))
      }

      window.addEventListener('resize', this.documentResizeListener)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.documentResizeListener)
    }

    crop () {
      const url = this.getAttribute('elm-url')
      const imageWidth = parseFloat(this.getAttribute('elm-image-width'))
      const imageHeight = parseFloat(this.getAttribute('elm-image-height'))
      const selectionLeft = parseFloat(this.getAttribute('elm-selection-left'))
      const selectionTop = parseFloat(this.getAttribute('elm-selection-top'))
      const selectionWidth = parseFloat(this.getAttribute('elm-selection-width'))
      const selectionHeight = parseFloat(this.getAttribute('elm-selection-height'))

      if (!(url || imageWidth || imageHeight || selectionLeft || selectionTop || selectionWidth || selectionHeight)) {
        return
      }

      const image = new Image()
      image.crossOrigin = 'anonymous'

      image.addEventListener('load', (e) => {
        const width = e.target.width
        const height = e.target.height

        const canvas = document.createElement('canvas')
        canvas.width = selectionWidth * width / imageWidth
        canvas.height = selectionHeight * height / imageHeight

        const context = canvas.getContext('2d')
        context.drawImage(
          image,
          selectionLeft * width / imageWidth,
          selectionTop * height / imageHeight,
          selectionWidth * width / imageWidth,
          selectionHeight * height / imageHeight,
          0,
          0,
          selectionWidth * width / imageWidth,
          selectionHeight * height / imageHeight
        )

        canvas.toBlob((blob) => {
          this.dispatchEvent(new CustomEvent('crop-image', {
            detail: {
              image: new File([blob], 'cropped-image.png')
            }
          }))
        }, 'image/png', 1)
      })

      image.src = url
    }
  }
)
