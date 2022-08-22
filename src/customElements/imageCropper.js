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
      const absoluteImageLeft = parseFloat(this.getAttribute('elm-image-left'))
      const absoluteImageTop = parseFloat(this.getAttribute('elm-image-top'))
      const containerLeft = parseFloat(this.getAttribute('elm-container-left'))
      const containerTop = parseFloat(this.getAttribute('elm-container-top'))
      const selectionLeft = parseFloat(this.getAttribute('elm-selection-left'))
      const selectionTop = parseFloat(this.getAttribute('elm-selection-top'))
      const selectionWidth = parseFloat(this.getAttribute('elm-selection-width'))
      const selectionHeight = parseFloat(this.getAttribute('elm-selection-height'))

      const allInputValues = {
        url,
        imageWidth,
        imageHeight,
        absoluteImageLeft,
        absoluteImageTop,
        containerTop,
        containerLeft,
        selectionLeft,
        selectionTop,
        selectionWidth,
        selectionHeight
      }

      if (Object.values(allInputValues).some(x => x !== 0 && !x)) {
        console.error('ImageCropper: missing input values', { cause: Object.keys(allInputValues).filter(x => !allInputValues[x]) })
        throw new Error('ImageCropper: missing input values', { cause: Object.keys(allInputValues).filter(x => !allInputValues[x]) })
      }

      const imageLeft = absoluteImageLeft - containerLeft
      const imageTop = absoluteImageTop - containerTop

      const image = new Image()
      image.crossOrigin = 'anonymous'

      image.addEventListener('load', (e) => {
        // The displayed image might not be as big as the original image
        // This is because if we display the image as-is, it might be too large to fit in the view
        const originalImageWidth = e.target.width
        const originalImageHeight = e.target.height

        // These ratios help us translate between the shown image and the original image
        const imageWidthRatio = originalImageWidth / imageWidth
        const imageHeightRatio = originalImageHeight / imageHeight

        const canvas = document.createElement('canvas')
        canvas.width = selectionWidth * imageWidthRatio
        canvas.height = selectionHeight * imageHeightRatio

        const context = canvas.getContext('2d')

        const sourceLeft = (selectionLeft - imageLeft) * imageWidthRatio
        const sourceTop = (selectionTop - imageTop) * imageHeightRatio
        const sourceWidth = selectionWidth * imageWidthRatio
        const sourceHeight = selectionHeight * imageHeightRatio

        context.drawImage(
          image,
          sourceLeft,
          sourceTop,
          sourceWidth,
          sourceHeight,
          0,
          0,
          canvas.width,
          canvas.height
        )

        canvas.toBlob((blob) => {
          this.dispatchEvent(new CustomEvent('crop-image', {
            detail: {
              image: new File([blob], 'cropped-image.png')
            }
          }))
        }, 'image/png', 1)
      })

      image.src = `${url}?invalidatecache=${Date.now()}`
    }
  }
)
