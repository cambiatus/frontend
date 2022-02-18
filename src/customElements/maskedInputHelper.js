/* global HTMLElement */

export default () => (
  class MaskedInputHelper extends HTMLElement {
    static get observedAttributes () { return ['decimal-separator'] }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'decimal-separator') {
        this._decimalSeparator = newValue
      }
    }

    connectedCallback () {
      this.className = 'hidden'

      const targetElement = document.getElementById(this.getAttribute('target-id'))
      if (!targetElement) throw new Error('Couldn\'t find target element for masked-input-helper')

      const maskType = this.getAttribute('mask-type')
      let previousSelectionStart = targetElement.selectionStart || 0
      let previousSelectionEnd = targetElement.selectionEnd || 0
      let previousValue = targetElement.value || ''

      this.inputListener = (e) => {
        window.setTimeout(() => {
          const newSelectionStart = targetElement.selectionStart || 0
          const isAtEnd = maskType === 'string' ? previousSelectionStart === previousValue.length : false
          const isDeletingNumber = e.data === null && maskType === 'number'
          const isChangingMask = Math.abs(previousSelectionStart - newSelectionStart) > 1

          if (maskType === 'number') {
            if (e.data === this._decimalSeparator) {
              if (targetElement.value.indexOf(this._decimalSeparator) === -1) return

              const newSelectionStart = targetElement.value.indexOf(this._decimalSeparator) + 2
              previousSelectionStart = newSelectionStart
              previousSelectionEnd = newSelectionStart
              previousValue = targetElement.value
              targetElement.setSelectionRange(newSelectionStart, newSelectionStart)
              return
            }

            if (previousSelectionStart === previousValue.indexOf(this._decimalSeparator) + 1 && !isDeletingNumber) {
              previousValue = targetElement.value
              previousSelectionStart = targetElement.value.indexOf(this._decimalSeparator)
              previousSelectionEnd = previousSelectionStart

              targetElement.setSelectionRange(previousSelectionStart, previousSelectionStart)
              return
            }
          }

          if (Math.abs(previousSelectionStart - previousSelectionEnd) > 0) {
            const newSelectionIndex = Math.max(previousSelectionStart, previousSelectionEnd) + 1
            targetElement.setSelectionRange(newSelectionIndex, newSelectionIndex)

            previousValue = targetElement.value
            previousSelectionStart = newSelectionIndex
            previousSelectionEnd = newSelectionIndex
            return
          }

          if ((isDeletingNumber || isChangingMask) && !isAtEnd) {
            const sumFactor = e.data !== null && targetElement.value.length >= previousValue.length
              ? +1
              : maskType === 'number' ? 0 : -1
            const newIndex = this.firstIndexAfter(targetElement.value, previousSelectionStart, e.data) + sumFactor
            targetElement.setSelectionRange(newIndex, newIndex)

            previousValue = targetElement.value
            previousSelectionStart = newIndex
            previousSelectionEnd = newIndex
            return
          }

          previousSelectionStart = newSelectionStart
          previousValue = targetElement.value
        }, 0)
      }

      this.keyDownListener = (e) => {
        let originalSelectionStart = targetElement.selectionStart
        if (Math.abs(targetElement.selectionStart - previousSelectionStart) < 2 || e.ctrlKey) {
          window.setTimeout(() => {
            if (Math.abs(targetElement.selectionStart - originalSelectionStart) < 2 || e.ctrlKey) {
              previousSelectionStart = targetElement.selectionStart
              previousSelectionEnd = targetElement.selectionEnd
            }
          }, 0)
        }
      }

      this.clickListener = () => {
        previousSelectionStart = targetElement.selectionStart
        previousSelectionEnd = targetElement.selectionEnd
      }

      targetElement.addEventListener('input', this.inputListener)
      targetElement.addEventListener('keydown', this.keyDownListener)
      targetElement.addEventListener('click', this.clickListener)
    }

    disconnectedCallback () {
      const targetElement = document.getElementById(this.getAttribute('target-id'))

      if (!targetElement) return

      targetElement.removeEventListener('input', this.inputListener)
      targetElement.removeEventListener('keydown', this.keyDownListener)
      targetElement.removeEventListener('click', this.clickListener)
    }

    firstIndexAfter (stringValue, baseIndex, element) {
      for (let index = 0; index < stringValue.length; index++) {
        if (stringValue[index] === element && index >= baseIndex) {
          return index
        }
      }

      return baseIndex
    }
  }
)
