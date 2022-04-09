/* global HTMLElement */

export default () => (
  class MasonryLayout extends HTMLElement {
    connectedCallback () {
      this.resizeItems = () => {
        this.childNodes.forEach((item) => this.resizeItem(item))
      }

      if (this.getAttribute('elm-transition-with-parent') === 'true') {
        this.transitionWithParent()
      }

      window.addEventListener('resize', this.resizeItems)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.resizeItems)
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
      const rowGap = parseInt(window.getComputedStyle(this).getPropertyValue('grid-row-gap'))
      const rowHeight = parseInt(window.getComputedStyle(this).getPropertyValue('grid-auto-rows'))
      const currentHeight = item.getBoundingClientRect().height
      const marginBottom = parseInt(window.getComputedStyle(item).getPropertyValue('margin-bottom'))

      const rowSpan = Math.ceil((currentHeight + rowGap + marginBottom) / (rowHeight + rowGap))

      item.style.gridRowEnd = 'span ' + rowSpan
    }
  }
)
