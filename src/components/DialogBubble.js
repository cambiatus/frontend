/* global HTMLElement */
class DialogBubble extends HTMLElement {
  constructor () {
    super()

    this._defaultClasses = 'absolute transform cursor-auto z-50 p-6 bg-white flex rounded shadow-2xl'
    this._defaultPointClasses = 'absolute transform -z-10'

    window.addEventListener('scroll', () => { this.setPosition() }, { passive: true })
    window.addEventListener('resize', () => { this.setPosition() }, { passive: true })
  }

  connectedCallback () {
    this.className = this._defaultClasses
    const point = document.createElement('div')
    const pointElement = document.createElement('div')
    pointElement.className = 'w-8 h-8 bg-white transform -rotate-45 rounded-sm'

    point.appendChild(pointElement)

    point.className = this._defaultPointClasses
    this.appendChild(point)

    this._point = point

    this._orientation = 'not defined'

    this.setPosition()
  }

  setPosition () {
    if (!this.isConnected) return
    let orientation = ''

    const element = this.getBoundingClientRect()
    const parent = this.parentElement.getBoundingClientRect()

    const minWidth = parseInt(this.getAttribute('elm-min-width'))
    const width = element.width > minWidth ? element.width : minWidth

    let positionClasses = ''

    let pointPositionClasses = ''

    if (parent.left <= width / 2) {
      // Go to the right
      positionClasses = 'left-full bottom-1/2 translate-y-1/2'
      pointPositionClasses = '-left-1 top-1/2 -translate-y-1/2'
      orientation = 'right'
    } else if (parent.right + (width / 2) >= window.innerWidth) {
      // Go to the left
      positionClasses = 'right-full bottom-1/2 translate-y-1/2'
      pointPositionClasses = '-right-1 top-1/2 -translate-y-1/2'
      orientation = 'left'
    } else if (parent.top <= element.height) {
      // Go down
      positionClasses = 'top-full right-1/2 translate-x-1/2'
      pointPositionClasses = '-top-1 right-1/2 translate-x-1/2'
      orientation = 'down'
    } else {
      // Go up
      positionClasses = 'bottom-full right-1/2 translate-x-1/2'
      pointPositionClasses = '-bottom-1 right-1/2 translate-x-1/2'
      orientation = 'up'
    }

    if (orientation === this._orientation) return

    this._orientation = orientation
    this.className = `${this.getAttribute('elm-class')} ${this._defaultClasses} ${positionClasses}`
    this._point.className = `${this._defaultPointClasses} ${pointPositionClasses}`
  }
}

export default DialogBubble
