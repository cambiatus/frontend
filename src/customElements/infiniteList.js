/* global HTMLElement, CustomEvent */

export default () => (
  class InfiniteList extends HTMLElement {
    static get observedAttributes () { return ['elm-distance-to-request', 'elm-element-to-track'] }

    connectedCallback () {
      this.listenToScroll()

      window.addEventListener('resize', this.listenToScroll)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.listenToScroll)
      clearInterval(this._scrollInterval)
    }

    attributeChangedCallback () {
      this.listenToScroll()
    }

    listenToScroll () {
      if (this._scrollInterval) {
        clearInterval(this._scrollInterval)
      }

      if (!this.getBoundingClientRect) {
        return
      }

      let scrolling = false
      const isHidden = this.getBoundingClientRect().width === 0 && this.getBoundingClientRect().height === 0
      if (isHidden) {
        return
      }

      const whatToTrack = this.getAttribute('elm-element-to-track')
      if (!whatToTrack) {
        return
      }

      let elementToTrack
      let elementToListen
      if (whatToTrack === 'track-window') {
        elementToTrack = this
        elementToListen = window
      } else if (whatToTrack === 'track-self') {
        elementToTrack = this
        elementToListen = this
      } else {
        elementToTrack = document.querySelector(whatToTrack)
        elementToListen = elementToTrack
      }

      if (!elementToTrack) {
        return
      }

      elementToListen.addEventListener('scroll', () => { scrolling = true })
      const distanceToRequest = parseInt(this.getAttribute('elm-distance-to-request')) || 0
      this._scrollInterval = setInterval(() => {
        if (scrolling) {
          scrolling = false
          if (elementToTrack.scrollTop >= elementToTrack.scrollHeight - distanceToRequest - elementToTrack.getBoundingClientRect().height) {
            this.dispatchEvent(new CustomEvent('requested-items', {}))
          }
        }
      }, 300)
    }
  }
)
