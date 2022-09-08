const addScript = () => {
  const _paq = window._paq || []

  /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
  _paq.push(['trackPageView'])
  _paq.push(['enableLinkTracking'])
  _paq.push(['setTrackerUrl', 'https://cambiatus.matomo.cloud/matomo.php'])
  _paq.push(['setSiteId', '5'])

  const script = document.createElement('script')
  script.async = true
  script.src = 'https://cdn.matomo.cloud/cambiatus.matomo.cloud/matomo.js'

  document.head.appendChild(script)
}

export { addScript }
