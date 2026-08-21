/* global HTMLElement, MutationObserver, ResizeObserver */

export default () => (
  class MasonryLayout extends HTMLElement {
    connectedCallback () {
      this.resizeItems = () => {
        for (const child of this.children) {
          this.resizeItem(child)
        }
      }

      // A child's height decides its row span, so re-measure whenever a child
      // is added (Elm appending a page of results) or changes height (an image
      // finishing loading, a pdf swapping its canvas in). This used to be a
      // `DOMNodeInserted` listener plus one-shot `load` listeners on the images
      // present at connect time. Mutation Events were removed in Chrome 127, so
      // appended children stopped being measured at all; and the `load`
      // listeners never matched anything, because a parent connects before its
      // children, so `<pdf-viewer>` had not created its `<img>` yet.
      this.childObserver = new ResizeObserver(entries => {
        for (const entry of entries) {
          this.resizeItem(entry.target)
        }
      })

      this.observeChildren = () => {
        for (const child of this.children) {
          this.childObserver.observe(child)
        }
      }

      this.childListObserver = new MutationObserver(() => {
        this.observeChildren()
        this.resizeItems()
      })

      if (this.getAttribute('elm-transition-with-parent') === 'true') {
        this.transitionWithParent()
      } else {
        this.resizeItems()
      }

      this.observeChildren()
      this.childListObserver.observe(this, { childList: true })

      window.addEventListener('resize', this.resizeItems)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.resizeItems)
      this.childListObserver.disconnect()
      this.childObserver.disconnect()
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
      const gridRowEnd = 'span ' + rowSpan

      // Only write when it actually changes: this runs from a ResizeObserver,
      // and an unconditional write on every callback is how those turn into
      // loops.
      if (item.style.gridRowEnd !== gridRowEnd) {
        item.style.gridRowEnd = gridRowEnd
      }
    }
  }
)
