/* global Notification */
// lets check if push is supported
function isPushSupported () {
  if (!('serviceWorker' in navigator)) return false
  if (!('PushManager' in window)) return false
  return true
}

// request push Permissions
function askPermission () {
  return new Promise(function (resolve, reject) {
    const permissionResult = Notification.requestPermission(function (result) {
      resolve(result)
    })

    if (permissionResult) {
      permissionResult.then(resolve, reject)
    }
  }).then(function (permissionResult) {
    if (permissionResult !== 'granted') {
      throw new Error("We weren't granted permission.")
    }
  })
}

// convert uint8array
function urlBase64ToUint8Array (base64String) {
  const padding = '='.repeat((4 - (base64String.length % 4)) % 4)
  const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/')
  const rawData = window.atob(base64)
  return Uint8Array.from([...rawData].map(char => char.charCodeAt(0)))
}

// subsribe user to Push
function subscribeUserToPush (pubKey) {
  const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`
  return navigator.serviceWorker
    .register(swUrl)
    .then(function (registration) {
      const subscribeOptions = {
        userVisibleOnly: true,
        applicationServerKey: urlBase64ToUint8Array(pubKey)
      }

      return registration.pushManager.subscribe(subscribeOptions)
    })
    .then(function (pushSubscription) {
      return pushSubscription
    })
}

// unsubscribe from push

function unsubscribeFromPush () {
  navigator.serviceWorker.ready
    .then(reg => reg.pushManager.getSubscription())
    .then(subscription => subscription.unsubscribe())
    .then(unregistered => {
      return unregistered
    })
    .catch(e => {
      return false
    })
}

export {
  subscribeUserToPush,
  askPermission,
  isPushSupported,
  unsubscribeFromPush
}
