/* global HTMLElement */

export default () => (
  class MasonryLayout extends HTMLElement {
    connectedCallback () {
      this.childNodes.forEach((item) => this.resizeItem(item))

      window.addEventListener('resize', this.resizeItems)
    }

    disconnectedCallback () {
      window.removeEventListener('resize', this.resizeItems)
    }

    resizeItems () {
      this.childNodes.forEach((item) => this.resizeItem(item))
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
