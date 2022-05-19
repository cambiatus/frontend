/* global HTMLElement, CustomEvent */

export default () => (
  class ImageCropper extends HTMLElement {
    static get observedAttributes () {
      return [
        'elm-image-url',
        'elm-image-width',
        'elm-image-height',
        'elm-selection-left',
        'elm-selection-top',
        'elm-selection-width',
        'elm-selection-height'
      ]
    }

    attributeChangedCallback () {
      this.crop()
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

        // const newUrl = canvas.toDataURL('image/png', 1)
        // this.dispatchEvent(new CustomEvent('crop-image', { detail: { newUrl } }))
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
