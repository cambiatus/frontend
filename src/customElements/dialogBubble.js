/* global HTMLElement */

export default () => (
  class DialogBubble extends HTMLElement {
    constructor () {
      super()

      this._defaultClasses = 'absolute transform cursor-auto z-50 p-6 bg-white flex rounded shadow-2xl'
      this._defaultPointClasses = 'absolute transform -z-10'

      window.addEventListener('resize', this.setPosition, { passive: true })
    }

    connectedCallback () {
      this.className = `${this.getAttribute('elm-class')} ${this._defaultClasses}`
      const bgClass = this.className.match(/bg-[a-z]*/)[0]

      const point = document.createElement('div')
      const pointElement = document.createElement('div')
      pointElement.className = `w-8 h-8 ${bgClass} transform -rotate-45 rounded-sm`

      const relativeSelector = this.getAttribute('elm-relative-selector')
      this._relativeElement = relativeSelector ? document.querySelector(relativeSelector) : null

      const scrollSelector = this.getAttribute('elm-scroll-selector')
      this._scrollSelector = scrollSelector ? document.querySelector(scrollSelector) : null

      if (this._scrollSelector !== null) {
        this._scrollSelector.addEventListener('scroll', this.setPosition, { passive: true })
      } else {
        window.addEventListener('scroll', this.setPosition)
      }

      point.appendChild(pointElement)

      point.className = this._defaultPointClasses
      this.appendChild(point)

      this._point = point

      this._sibling = this.previousSibling || this.nextSibling

      this.setPosition()
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.setPosition)
      if (this._scrollSelector !== null) {
        this._scrollSelector.removeEventListener('scroll', this.setPosition)
      } else {
        window.removeEventListener('scroll', this.setPosition)
      }
    }

    setPosition () {
      if (!this._sibling) return

      const relativeTop = this._relativeElement ? this._relativeElement.getBoundingClientRect().top : -window.scrollY
      const relativeLeft = this._relativeElement ? this._relativeElement.getBoundingClientRect().left : -window.scrollX

      const siblingRect = this._sibling.getBoundingClientRect()
      const siblingTop = siblingRect.top
      const siblingLeft = siblingRect.left
      const targetTop = siblingRect.top - relativeTop
      const targetLeft = siblingRect.left - relativeLeft
      const thisRect = this.getBoundingClientRect()

      let pointPositionClasses = ''
      let top = siblingTop
      let left = siblingLeft
      if (siblingLeft <= thisRect.width / 2) {
        // Go to the right
        top = targetTop - thisRect.height / 2 + siblingRect.height / 2
        left = targetLeft + siblingRect.width
        pointPositionClasses = '-left-1 top-1/2 -translate-y-1/2'
      } else if (siblingLeft + siblingRect.width + thisRect.width / 2 >= window.innerWidth) {
        // Go to the left
        top = targetTop - thisRect.height / 2 + siblingRect.height / 2
        left = targetLeft - thisRect.width
        pointPositionClasses = '-right-1 top-1/2 -translate-y-1/2'
      } else if (siblingTop <= thisRect.height) {
        // Go down
        top = targetTop + siblingRect.height
        left = targetLeft + siblingRect.width / 2 - thisRect.width / 2
        pointPositionClasses = '-top-1 right-1/2 translate-x-1/2'
      } else {
        // Go up
        top = targetTop - thisRect.height
        left = targetLeft + siblingRect.width / 2 - thisRect.width / 2
        pointPositionClasses = '-bottom-1 right-1/2 translate-x-1/2'
      }

      this.style.top = `${top}px`
      this.style.left = `${left}px`

      this._point.className = `${this._defaultPointClasses} ${pointPositionClasses}`
    }
  }
)
