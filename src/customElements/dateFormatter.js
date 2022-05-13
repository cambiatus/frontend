/* global HTMLElement */

export default () => (
  class DateFormatter extends HTMLElement {
    static get observedAttributes () { return ['elm-locale', 'elm-date'] }

    constructor () {
      super()

      const shadow = this.attachShadow({ mode: 'open' })
      const textContainer = document.createElement('span')
      this._textContainer = textContainer
      shadow.appendChild(textContainer)
    }

    connectedCallback () {
      this.setDateText()
    }

    attributeChangedCallback () {
      this.setDateText()
    }

    setDateText () {
      const locale = this.getAttribute('elm-locale')
      const date = new Date(parseInt(this.getAttribute('elm-date')))
      const dayString = date.toLocaleDateString(locale, { day: 'numeric' })
      const monthString = date.toLocaleDateString(locale, { month: 'short' })
        .replace(/[.]/g, '')
      const yearString = date.toLocaleDateString(locale, { year: 'numeric' })

      const translationString = this.getAttribute('elm-translation') === null ? '{{date}}' : this.getAttribute('elm-translation')
      const translatedString = translationString.replace(/{{date}}/, `${dayString} ${monthString} ${yearString}`)
      this._textContainer.textContent = translatedString
    }
  }
)
