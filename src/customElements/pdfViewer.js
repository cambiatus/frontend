/* global HTMLElement */

import * as pdfjsLib from 'pdfjs-dist/es5/build/pdf'

// If you're updating `pdfjs-dist`, make sure to
// `cp ./node_modules/pdfjs-dist/es5/build/pdf.worker.min.js ./public`
pdfjsLib.GlobalWorkerOptions.workerSrc = '/pdf.worker.min.js'

export default (app, config, addBreadcrumb) => (
  class PdfViewer extends HTMLElement {
    connectedCallback () {
      if (this.hasChildNodes()) return

      const url = this.getAttribute('elm-url')
      const childClass = this.getAttribute('elm-child-class')

      const loadingImg = document.createElement('img')
      loadingImg.src = '/images/loading.svg'
      this.appendChild(loadingImg)

      let setContent = (node) => {
        this.removeChild(loadingImg)
        this.appendChild(node)
      }

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

        setContent = (node) => {
          this.removeChild(loadingImg)
          this.removeChild(loadingTitle)
          this.removeChild(loadingSubtitle)
          this.appendChild(node)
        }
      } else {
        loadingImg.className = 'p-4'
      }

      const notFoundTimeout = window.setTimeout(() => {
        const notFoundImg = document.createElement('img')
        notFoundImg.src = '/icons/pdf.svg'
        setContent(notFoundImg)
        const bgColor = 'bg-purple-500'
        this.classList.add(bgColor)

        setContent = (node) => {
          this.removeChild(notFoundImg)
          if (this.classList.contains(bgColor)) {
            this.classList.remove(bgColor)
          }
          this.appendChild(node)
        }
      }, 1000 * 5)

      pdfjsLib.getDocument(url).promise.then((pdf) => {
        pdf.getPage(1).then((page) => {
          const canvas = document.createElement('canvas')
          canvas.className = childClass

          const width = this.clientWidth
          const height = this.clientHeight
          const unscaledViewport = page.getViewport({ scale: 1 })
          const scale = Math.min((height / unscaledViewport.height), (width / unscaledViewport.width))

          const viewport = page.getViewport({ scale })
          const canvasContext = canvas.getContext('2d')
          canvas.width = viewport.width
          canvas.height = viewport.height

          const renderContext = { canvasContext, viewport }

          const renderTask = page.render(renderContext)
          renderTask.promise.then(() => {
            window.clearTimeout(notFoundTimeout)
            setContent(canvas)
          })
        })
      }).catch((e) => {
        const invalidPDFError = 'Invalid PDF structure.'
        if (e.message === invalidPDFError) {
          const img = document.createElement('img')
          img.src = url
          img.className = childClass
          window.clearTimeout(notFoundTimeout)
          setContent(img)
        } else {
          addBreadcrumb({
            type: 'error',
            category: 'pdf-viewer',
            message: 'Got an error when trying to display a PDF',
            data: { error: e },
            localData: {},
            level: 'warning'
          })
        }
      })
    }
  }
)
