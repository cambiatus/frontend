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
  const payload = event.data.json()

  console.log('payload', payload)
  let title
  let body
  let interaction

  try {
    title = payload.title
    body = payload.title
    interaction = true
  } catch (e) {
    // TODO find a way of reporting error without sentry

    // Set interaction to none if the notificatiion is bungled
    interaction = false
  } finally {
    body = body === undefined ? "Huh! You shouldn't be getting this" : body
    title = title === undefined ? '' : title
  }

  const options = {
    body: body,
    icon: '/images/logo-cambiatus.svg',
    requireInteraction: interaction,
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
