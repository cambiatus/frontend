/* global HTMLElement */

export default () => (
  class MasonryLayout extends HTMLElement {
    connectedCallback () {
      this.resizeItems = () => {
        for (const child of this.children) {
          this.resizeItem(child)
        }
      }

      if (this.getAttribute('elm-transition-with-parent') === 'true') {
        this.transitionWithParent()
      } else {
        this.resizeItems()
      }

      this.querySelectorAll('img').forEach((image) =>
        image.addEventListener('load', () => this.resizeItems()))

      window.addEventListener('resize', this.resizeItems)

      this.domNodeListener = (e) => {
        this.resizeItem(e.target)
      }

      this.addEventListener('DOMNodeInserted', this.domNodeListener)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.resizeItems)
      this.removeEventListener('DOMNodeInserted', this.domNodeListener)
    }

    transitionWithParent () {
      const transitionContainer = this.parentElement

      const onTransitionStart = () => {
        let isTransitioning = true
        const onTransitionEnd = () => {
          isTransitioning = false
          this.resizeItems()
          transitionContainer.removeEventListener('transitionend', onTransitionEnd)
        }

        transitionContainer.addEventListener('transitionend', onTransitionEnd)

        const onAnimationFrame = () => {
          if (!isTransitioning) {
            return
          }

          this.resizeItems()
          window.requestAnimationFrame(onAnimationFrame)
        }
        window.requestAnimationFrame(onAnimationFrame)
      }

      transitionContainer.addEventListener('transitionstart', onTransitionStart)
    }

    resizeItem (item) {
      if (!item || !item.getBoundingClientRect) {
        return
      }

      const rowGap = parseInt(window.getComputedStyle(this).getPropertyValue('grid-row-gap'))
      const rowHeight = parseInt(window.getComputedStyle(this).getPropertyValue('grid-auto-rows'))
      const currentHeight = item.getBoundingClientRect().height
      const marginBottom = parseInt(window.getComputedStyle(item).getPropertyValue('margin-bottom'))

      const rowSpan = Math.ceil((currentHeight + rowGap + marginBottom) / (rowHeight + rowGap))

      item.style.gridRowEnd = 'span ' + rowSpan
    }
  }
)
