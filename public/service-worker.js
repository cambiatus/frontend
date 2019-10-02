var CACHE_NAME = 'bes-site-cache-v1'
var urlsToCache = [
  '/'
]

// install event handler here setup our local landscape
self.addEventListener('install', function (event) {
  self.skipWaiting()
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(function (cache) {
        return cache.addAll(urlsToCache)
      })
  )
})

/* global self, caches, fetch  */
// fetch handler
self.addEventListener('fetch', function (event) {
  event.respondWith(
    caches.match(event.request).then(function (response) {
      if (response) {
        return response
      }
      return fetch(event.request)
    })
  )
})

// Configure push eventListener to handle C2DM messages
self.addEventListener('push', function (event) {
  let title = null
  let body = null
  let payload

  try {
    payload = event.data.json()
    title = payload.title
    body = payload.title
  } catch (e) {
    // TODO find a way of reporting error without sentry
  } finally {
    body = body === null ? payload.body : body
    title = title === null ? payload.title : title
  }

  const options = {
    body: body,
    icon: '/images/logo-cambiatus.svg',
    requireInteraction: true,
    vibrate: [200, 100, 200, 100, 200, 100, 400],
    tag: title
  }

  const notificationChain = self.registration.showNotification(title, options)

  event.waitUntil(notificationChain)
})

// Handle Notification Clicks
self.addEventListener('notificationclick', function (event) {
  const notification = event.notification

  notification.close()

  // TODO Navigate user to specific notification page
})
