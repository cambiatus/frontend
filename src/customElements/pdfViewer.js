/* global HTMLElement, CustomEvent */

import * as pdfjsLib from 'pdfjs-dist/es5/build/pdf'

// If you're updating `pdfjs-dist`, make sure to
// `cp ./node_modules/pdfjs-dist/es5/build/pdf.worker.min.js ./public`
pdfjsLib.GlobalWorkerOptions.workerSrc = '/pdf.worker.min.js'

export default (app, config, addBreadcrumb) => (
  class PdfViewer extends HTMLElement {
    connectedCallback () {
      if (this.hasChildNodes()) return

      this._url = this.getAttribute('elm-url')
      this._childClass = this.getAttribute('elm-child-class')

      const img = document.createElement('img')
      img.src = this._url
      img.className = this._childClass
      this.appendChild(img)

      img.addEventListener('load', () => {
        this.dispatchEvent(new CustomEvent('file-type-discovered', { detail: 'image' }))
      })

      img.addEventListener('error', async () => {
        this.dispatchEvent(new CustomEvent('file-type-discovered', { detail: 'pdf' }))
        this.removeChild(img)
        this.appendLoadingImage()

        const pdfDocument = await pdfjsLib.getDocument(this._url).promise
        const firstPage = await pdfDocument.getPage(1)

        const canvas = document.createElement('canvas')
        canvas.className = this._childClass

        const width = this.clientWidth
        const height = this.clientHeight
        const unscaledViewport = firstPage.getViewport({ scale: 1 })
        const scale = Math.min(height / unscaledViewport.height, width / unscaledViewport.width)

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

    appendLoadingImage () {
      const loadingImg = document.createElement('img')
      loadingImg.src = '/images/loading.svg'

      this.appendChild(loadingImg)

      if (this.getAttribute('elm-loading-title') && this.getAttribute('elm-loading-subtitle')) {
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
)
