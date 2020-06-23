var CACHE_NAME = 'bes-site-cache-v1-hotfix'

// See https://github.com/cambiatus/frontend/issues/252 for the details
var urlsToCache = []
self.caches.delete('bes-site-cache-v1')

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
  var payload = {}

  if (event.data) {
    payload = event.data.json()
  }

  var body = payload.body || 'Huh! You should npt be getting this'
  var title = payload.title || 'Broken notification'
  var interaction
  if (title === 'Broken notification') {
    interaction = false
  } else {
    interaction = true
  }

  const options = {
    body: body,
    icon: 'images/logo-cambiatus.svg',
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

  // Open the notifications page
  event.waitUntil(self.clients.matchAll({
    type: 'window'
  }).then(function (clientList) {
    for (var i = 0; i < clientList.length; i++) {
      var client = clientList[i]
      // If notifications page is open and focused do nothing
      if (client.url === '/notification' && 'focus' in client) { return client.focus() }
    }
    // Otherwise open the client on the notifications page
    if (self.clients.openWindow) { return self.clients.openWindow('/notification') }
  }))
})
