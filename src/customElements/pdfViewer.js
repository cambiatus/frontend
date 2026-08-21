/* global HTMLElement, CustomEvent */

import * as pdfjsLib from 'pdfjs-dist'

// If you're updating `pdfjs-dist`, make sure to
// `cp ./node_modules/pdfjs-dist/build/pdf.worker.min.js ./public`
pdfjsLib.GlobalWorkerOptions.workerSrc = '/pdf.worker.min.js'

export default (app, config, addBreadcrumb) =>
  class PdfViewer extends HTMLElement {
    static get observedAttributes () { return ['elm-url', 'elm-child-class'] }

    connectedCallback () {
      this.render()
    }

    // Elm reuses the same `<pdf-viewer>` DOM node across different claims in a
    // list, patching only the `elm-url` attribute. Without reacting to that
    // change the node keeps rendering the previous claim's image, so re-render
    // whenever the url (or child class) changes after the element is connected.
    attributeChangedCallback (name, oldValue, newValue) {
      if (this.isConnected && oldValue !== newValue) {
        this.render()
      }
    }

    render () {
      const url = this.getAttribute('elm-url')
      const childClass = this.getAttribute('elm-child-class')

      // Nothing to do if we've already rendered this exact url.
      if (this._renderedUrl === url && this.hasChildNodes()) return

      this._renderedUrl = url
      this._childClass = childClass
      this.clearChildren()

      const img = document.createElement('img')
      img.src = url
      img.className = childClass
      this.appendChild(img)

      img.addEventListener('load', () => {
        this.dispatchEvent(
          new CustomEvent('file-type-discovered', { detail: 'image' })
        )
      })

      img.addEventListener('error', async () => {
        // A newer render may have replaced this img before it errored out.
        if (this._renderedUrl !== url) return

        this.dispatchEvent(
          new CustomEvent('file-type-discovered', { detail: 'pdf' })
        )
        this.removeChild(img)
        this.appendLoadingImage()

        const pdfDocument = await pdfjsLib.getDocument(url).promise
        const firstPage = await pdfDocument.getPage(1)

        // Bail if the url changed while we were loading the pdf.
        if (this._renderedUrl !== url) return

        const canvas = document.createElement('canvas')
        canvas.className = childClass

        const width = this.clientWidth
        const height = this.clientHeight
        const unscaledViewport = firstPage.getViewport({ scale: 1 })
        const scale = Math.min(
          height / unscaledViewport.height,
          width / unscaledViewport.width
        )

        const viewport = firstPage.getViewport({ scale })
        const canvasContext = canvas.getContext('2d')
        canvas.width = viewport.width
        canvas.height = viewport.height

        const renderContext = { canvasContext, viewport }

        await firstPage.render(renderContext).promise

        this.removeLoadingImage()
        this.appendChild(canvas)
      })
    }

    clearChildren () {
      if (this.removeLoadingImage) {
        this.removeLoadingImage = null
      }
      while (this.firstChild) {
        this.removeChild(this.firstChild)
      }
    }

    appendLoadingImage () {
      const loadingImg = document.createElement('img')
      loadingImg.src = '/images/loading.svg'

      this.appendChild(loadingImg)

      if (
        this.getAttribute('elm-loading-title') &&
        this.getAttribute('elm-loading-subtitle')
      ) {
        loadingImg.className = 'h-16 mt-8'

        const loadingTitle = document.createElement('p')
        loadingTitle.className = 'font-bold text-2xl'
        loadingTitle.textContent = this.getAttribute('elm-loading-title')
        this.appendChild(loadingTitle)

        const loadingSubtitle = document.createElement('p')
        loadingSubtitle.className = 'text-sm'
        loadingSubtitle.textContent = this.getAttribute('elm-loading-subtitle')
        this.appendChild(loadingSubtitle)

        this.removeLoadingImage = () => {
          this.removeChild(loadingImg)
          this.removeChild(loadingTitle)
          this.removeChild(loadingSubtitle)
        }
      } else {
        loadingImg.className = 'p-4'
        this.removeLoadingImage = () => {
          this.removeChild(loadingImg)
        }
      }
    }
  }
