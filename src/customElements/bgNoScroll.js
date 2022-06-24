/* global HTMLElement */

export default () => (
  class BgNoScroll extends HTMLElement {
    constructor () {
      super()

      this._preventScrollingClasses = []
      this._classesHandledByOthers = []
    }

    connectedCallback () {
      const preventScrollingClasses = this.getAttribute('elm-prevent-scroll-class')

      this._preventScrollingClasses = preventScrollingClasses.split(' ')
      this._preventScrollingClasses.forEach((class_) => {
        if (document.body.classList.contains(class_)) {
          this._classesHandledByOthers.push(class_)
          return
        }

        document.body.classList.add(class_)
      })
    }

    disconnectedCallback () {
      this._preventScrollingClasses.forEach((class_) => {
        if (this._classesHandledByOthers.includes(class_) || !document.body.classList.contains(class_)) {
          return
        }

        document.body.classList.remove(class_)
      })
    }
  }
)
