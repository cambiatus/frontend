/* global HTMLElement, CustomEvent */

import Quill from 'quill'
import QuillDelta from 'quill-delta'

import usePortAsPromise from '../utils/usePortAsPromise'

export default (app) => (

  class RichTextEditor extends HTMLElement {
    static get observedAttributes () { return ['elm-edit-text', 'elm-remove-text', 'elm-disabled', 'elm-has-error'] }

    constructor () {
      super()

      this._quillContainer = document.createElement('div')
      this._parentContainer = document.createElement('div')
      this._parentContainer.className = 'placeholder-gray-400 input-border'

      this._parentContainer.appendChild(this._quillContainer)
    }

    connectedCallback () {
      this._quill = new Quill(this._quillContainer,
        {
          modules: {
            toolbar: [
              [{ 'header': 1 }, { 'header': 2 }],
              ['bold', 'italic', 'strike', 'underline'],
              ['link'],
              [{ 'list': 'ordered' }, { 'list': 'bullet' }]
            ]
          },
          formats: ['header', 'bold', 'code', 'italic', 'link', 'strike', 'underline', 'list'],
          placeholder: this.getAttribute('elm-placeholder'),
          theme: 'snow'
        }
      )

      this.setMarkdownListener = (data) => {
        if (data.id === this.getId()) {
          this._quill.setContents(data.content)
        }
      }

      app.ports.setMarkdown.subscribe(this.setMarkdownListener)

      this._quill.on('text-change', (delta, oldDelta, source) => {
        const contents = this._quill.getContents()
        this.dispatchEvent(new CustomEvent('text-change', { detail: contents }))
      })

      const toolbar = this._quill.getModule('toolbar')
      toolbar.addHandler('link', () => { this.linkHandler() })

      // If we dont include the timeout, we get some annoying bugs in
      // development where the text is cleared, and hot reloading bugs out and
      // crashes the app
      window.setTimeout(() => {
        this.dispatchEvent(new CustomEvent('component-loaded', {}))
      }, 0)
      this.appendChild(this._parentContainer)

      // Remove default click handler and add our custom one
      const oldEditButton = this.querySelector('.ql-tooltip a.ql-action')
      const editButton = oldEditButton.cloneNode(true)
      oldEditButton.parentNode.replaceChild(editButton, oldEditButton)
      editButton.addEventListener('click', (e) => {
        this.querySelector('.ql-tooltip').classList.add('ql-hidden')
        this.linkHandler()
      })

      this.setId()
      this.setTooltipTexts()
      this.setDisabled()
    }

    disconnectedCallback () {
      app.ports.setMarkdown.unsubscribe(this.setMarkdownListener)
    }

    attributeChangedCallback (name, oldValue) {
      if (name === 'elm-disabled') {
        // If oldValue is null, it means this is the first render, and this
        // function is called before this.connectedCallback, so this._quill
        // hasn't been initialized (and we get an error). We call
        // this.setDisabled on this.connectedCallback to disable quill initially
        if (oldValue === null) {
          return
        }

        this.setDisabled()
      } else if (name === 'elm-has-error') {
        this.toggleHasError()
      } else if (name === 'elm-id') {
        this.setId()
      } else {
        this.setTooltipTexts()
      }
    }

    setId () {
      const editorNode = this.querySelector('.ql-editor')
      editorNode.id = this.getAttribute('elm-id')
    }

    getId () {
      return this.getAttribute('elm-id')
    }

    setTooltipTexts () {
      const actionButton = this.querySelector('.ql-tooltip a.ql-action')
      if (actionButton) {
        actionButton.setAttribute('data-edit-text', this.getAttribute('elm-edit-text'))
      }

      const removeButton = this.querySelector('.ql-tooltip a.ql-remove')
      if (removeButton) {
        removeButton.setAttribute('data-remove-text', this.getAttribute('elm-remove-text'))
      }
    }

    toggleHasError () {
      const hasError = this.getAttribute('elm-has-error') === 'true'
      if (hasError) {
        this._parentContainer.classList.add('with-error')
      } else {
        this._parentContainer.classList.remove('with-error')
      }
    }

    setDisabled () {
      const isDisabled = this.getAttribute('elm-disabled') === 'true'
      const toolbarButtons = this.querySelectorAll('.ql-toolbar.ql-snow button')

      if (isDisabled) {
        this._quill.disable()
        toolbarButtons.forEach((toolbarButton) => {
          toolbarButton.disabled = true
        })
      } else {
        this._quill.enable()
        toolbarButtons.forEach((toolbarButton) => {
          toolbarButton.disabled = false
        })
      }
    }

    linkHandler () {
      if (!this._quill.isEnabled()) {
        return
      }

      let range = this._quill.getSelection(true)
      const isLink = this._quill.getFormat(range).link !== undefined
      if (range.length === 0 && isLink) {
        range = this.getFormattedRange(range.index)
      }
      const text = this._quill.getText(range)
      const currentFormat = this._quill.getFormat(range)

      usePortAsPromise(app.ports.markdownLink, (link) => {
        if (link.id === this.getId()) {
          this._quill.updateContents(new QuillDelta()
            .retain(range.index)
            .delete(range.length)
            .insert(link.label, { ...currentFormat, link: link.url })
          )

          this._quill.setSelection(range.index + link.label.length, 0, 'silent')

          return { unsubscribeFromPort: true }
        } else {
          return { unsubscribeFromPort: false }
        }
      })

      this.dispatchEvent(new CustomEvent('clicked-include-link',
        {
          detail: {
            label: text,
            url: currentFormat.link || ''
          }
        }
      ))
    }

    /** Gets the range from the formatting that the `index` position is affected
    by. Useful when the user has their caret in the middle of a link, but isn't
    actually selecting it (selection length is 0)
    */
    getFormattedRange (index) {
      let currentIndex = 0
      for (const content of this._quill.getContents().ops) {
        const initialIndex = currentIndex
        const finalIndex = initialIndex + content.insert.length - 1
        currentIndex = finalIndex + 1
        if (initialIndex <= index && index <= finalIndex) {
          return { index: initialIndex, length: finalIndex - initialIndex + 1 }
        }
      }
      return { index: 0, length: 0 }
    }
  }
)
