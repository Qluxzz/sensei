/**
 * This service worker allows the client side app to work even if we're offline
 * by caching the resources used for the app
 */
const CACHE_NAME = "DEFAULT_CACHE_NAME"
const PRE_CACHED_RESOURCES = ["", "manifest.json", "style.css", "elm.js"]

self.addEventListener("install", (e) => {
  e.waitUntil(
    caches.open(CACHE_NAME).then((cache) => cache.addAll(PRE_CACHED_RESOURCES))
  )
})

self.addEventListener("fetch", (event) => {
  if (shouldCache(event.request.url))
    event.respondWith(cacheFirst(event.request))
})

/**
 * @param {Request} request
 */
async function cacheFirst(request) {
  // The query is only read by the app so we want to ignore the query when caching
  // so we don't cache the same thing multiple times
  const urlWithoutQuery = stripQueryFromUrl(request.url)

  const cachedResponse = await caches.match(urlWithoutQuery)
  if (cachedResponse) {
    return cachedResponse
  }
  try {
    const networkResponse = await fetch(request)
    if (networkResponse.ok) {
      const cache = await caches.open(CACHE_NAME)
      cache.put(urlWithoutQuery, networkResponse.clone())
    }
    return networkResponse
  } catch (error) {
    return Response.error()
  }
}

/**
 * @param {string} urlString
 */
function shouldCache(urlString) {
  const urlWithoutQuery = stripQueryFromUrl(urlString)

  return PRE_CACHED_RESOURCES.some(
    (resource) => new Request(`./${resource}`).url === urlWithoutQuery
  )
}

/**
 * @param {string} urlString
 */
function stripQueryFromUrl(urlString) {
  const url = new URL(urlString)

  return `${url.origin}${url.pathname}`
}
