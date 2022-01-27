/**
 * Subscribe to a port to consume it only once and unsubcribe from it.
 * It's good practice to have an id to check if that port is actually supposed
 * to communicate with the component that requested it. In case it's not, the
 * `handler` should return `{ unsubscribeFromPort: false }` (that way the
 * component doesn't unsubscribe from the port)
 * @param {*} port an elm port
 * @param {*} handler a function to act as the port's subscription
 * @returns a promise that resolves with the result of `handler`
 */
const usePortAsPromise = (port, handler) => {
  return new Promise((resolve, reject) => {
    const internalHandler = (...args) => {
      const result = handler(...args)
      if (result && (result.unsubcribeFromPort === undefined || result.unsubscribeFromPort !== false)) {
        port.unsubscribe(internalHandler)
        resolve(result)
      }
    }

    port.subscribe(internalHandler)
  })
}

export default usePortAsPromise
