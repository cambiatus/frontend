/* global HTMLElement, CustomEvent, IntersectionObserver */

export default () => (
  class IntersectionObserverElement extends HTMLElement {
    connectedCallback () {
      const targetSelectors = this.getAttribute('elm-target').split(' ')
      const threshold = parseFloat(this.getAttribute('elm-threshold')) || 1

      const options = { threshold }

      this.targets = targetSelectors
        .map(selector => document.querySelector(selector))
        .filter((target) => target !== null)

      if (!this.targets) {
        console.error('INVALID TARGETS FOR INTERSECTION OBSERVER')
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
          }

          wasIntersecting[entry.target.id] = entry.isIntersecting
        })
      }
      this.observer = new IntersectionObserver(this.callback, options)

      this.targets.forEach((target) => this.observer.observe(target))
    }

    disconnectedCallback () {
      if (this.targets) {
        this.targets.forEach((target) => this.observer.unobserve(target))
      }

      this.observer.disconnect()
    }
  }
)
