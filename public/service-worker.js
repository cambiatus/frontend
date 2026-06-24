/* global self, caches */

// Self-healing service worker. See https://github.com/cambiatus/frontend/issues/252
//
// Old versions precached the app shell. After a deploy the hashed asset names
// change, so a client serving the stale cached index.html requested JS that no
// longer existed (404) and froze on the splash screen — most visibly in Safari.
// This SW caches nothing, purges every previous cache, takes control of open
// tabs immediately, and reloads any window still running the stale bundle, so
// users recover automatically without clearing site data by hand.

self.addEventListener('install', function (event) {
  // Replace the old SW as soon as this one installs.
  self.skipWaiting()
})

self.addEventListener('activate', function (event) {
  event.waitUntil(
    (async function () {
      // Drop every cache left behind by any previous SW version.
      const names = await caches.keys()
      await Promise.all(names.map(function (name) { return caches.delete(name) }))

      // Control already-open tabs without waiting for a reload.
      await self.clients.claim()

      // Reload any window still showing the stale, frozen bundle so it fetches
      // the live assets from the network.
      const windows = await self.clients.matchAll({ type: 'window' })
      windows.forEach(function (client) {
        if ('navigate' in client) { client.navigate(client.url) }
      })
    })()
  )
})

// No fetch handler: never serve app assets from cache. Requests go straight to
// the network (HTTP caching still applies); the SW exists only for push.

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
