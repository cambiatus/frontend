/* global HTMLElement, CustomEvent, IntersectionObserver */

export default () => (
  class IntersectionObserverElement extends HTMLElement {
    static get observedAttributes () { return ['elm-target', 'elm-threshold'] }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'elm-target') {
        this.setTargets(newValue.split(' '))
      } else if (name === 'elm-threshold') {
        this.threshold = parseFloat(newValue) || 1
      }

      this.unobserve()
      this.observe()
    }

    setTargets (targetSelectors) {
      this.targets = targetSelectors
        .filter((targetSelector) => { return Boolean(targetSelector) })
        .map(selector => document.querySelector(selector))
        .filter((target) => target !== null)
    }

    isAboveMaxWidth () {
      const maxWidthAttribute = this.getAttribute('elm-max-width')
      if (maxWidthAttribute === 'none') {
        return false
      }

      const maxWidth = parseInt(maxWidthAttribute) || 0
      return window.innerWidth >= maxWidth
    }

    connectedCallback () {
      if (this.isAboveMaxWidth()) {
        return
      }

      const targetSelectors = this.getAttribute('elm-target').split(' ')
      this.setTargets(targetSelectors)

      this.threshold = parseFloat(this.getAttribute('elm-threshold')) || 1

      this.observe()
    }

    disconnectedCallback () {
      if (this.isAboveMaxWidth()) {
        return
      }

      this.unobserve()
    }

    observe () {
      if (this.isAboveMaxWidth()) {
        return
      }

      if (!this.targets) {
        console.error('Invalid targets for intersectionObserver')
        return
      }

      let wasIntersecting = this.targets.reduce(
        (currObj, currTarget) => {
          const newObject = currObj
          newObject[currTarget.id] = false
          return newObject
        },
        {}
      )

      this.callback = (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting && !wasIntersecting[entry.target.id]) {
            this.dispatchEvent(new CustomEvent('started-intersecting', { detail: { targetId: entry.target.id } }))
          } else if (!entry.isInterecting && wasIntersecting[entry.target.id]) {
            this.dispatchEvent(new CustomEvent('stopped-intersecting', { detail: { targetId: entry.target.id } }))
          }

          wasIntersecting[entry.target.id] = entry.isIntersecting
        })
      }

      this.observer = new IntersectionObserver(this.callback, {
        threshold: this.threshold,
        rootMargin: '100% 0px 100% 0px'
      })

      this.targets.forEach((target) => this.observer.observe(target))
    }

    unobserve () {
      if (!this.observer) {
        return
      }

      if (this.targets) {
        this.targets.forEach((target) => this.observer.unobserve(target))
      }

      this.observer.disconnect()
    }
  }
)
