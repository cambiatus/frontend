// Reads the browser's location for the member's place. Mirrors the LiveView hook
// (backend assets/js/hooks/member_location.js):
//
// - Permission `granted`: the position is read silently.
// - `prompt`: asked once per session; the browser's own prompt is the consent.
// - `denied`, or no Permissions API and no geolocation: the place follows the default
//   address ("unavailable").
//
// Throttled with sessionStorage: a position is sent at most once per session, or again
// when it moved more than ~1 km. Coordinates are rounded to ~110 m here and used for one
// lookup on the server, which never stores or logs them.
const KEY = "cambiatus-member-location";
const NOTE_KEY = "cambiatus-address-note-dismissed";
const MOVED_M = 1000;
const TIMEOUT_MS = 10000;
const MAX_AGE_MS = 300000;
const PERMISSION_DENIED = 1; // GeolocationPositionError.PERMISSION_DENIED

const coarse = (n) => Math.round(n * 1000) / 1000;

function read(key) {
  try {
    return JSON.parse(sessionStorage.getItem(key) || "null");
  } catch (_error) {
    return null;
  }
}

function write(key, value) {
  try {
    sessionStorage.setItem(key, JSON.stringify(value));
  } catch (_error) {
    // Private mode: no throttle, still correct.
  }
}

// Great-circle distance in metres (haversine).
function metres(a, b) {
  const rad = (d) => (d * Math.PI) / 180;
  const h =
    Math.sin(rad(b.lat - a.lat) / 2) ** 2 +
    Math.cos(rad(a.lat)) *
      Math.cos(rad(b.lat)) *
      Math.sin(rad(b.lng - a.lng) / 2) ** 2;
  return 2 * 6371000 * Math.asin(Math.sqrt(h));
}

async function permission() {
  try {
    return (
      (await navigator.permissions?.query({ name: "geolocation" }))?.state ??
      null
    );
  } catch (_error) {
    return null;
  }
}

function unavailable() {
  if (read(KEY)?.unavailable) return { kind: "skipped" };
  write(KEY, { unavailable: true });
  return { kind: "unavailable" };
}

function position() {
  return new Promise((resolve) => {
    navigator.geolocation.getCurrentPosition(
      ({ coords }) => resolve({ coords }),
      (error) => resolve({ error }),
      { enableHighAccuracy: false, timeout: TIMEOUT_MS, maximumAge: MAX_AGE_MS }
    );
  });
}

/**
 * Resolves to `{ kind: "located", lat, lng }`, `{ kind: "unavailable" }` (refused or
 * no geolocation) or `{ kind: "skipped" }` (nothing to send now: already asked this
 * session, not moved, timed out). Always also says whether the address note was
 * dismissed this session. Never rejects.
 */
export async function locate() {
  const noteDismissed = read(NOTE_KEY) === true;
  const result = await locateOnce();
  return { ...result, noteDismissed };
}

async function locateOnce() {
  if (!navigator.geolocation) return unavailable();

  const state = await permission();
  const last = read(KEY);

  if (state === "denied") return unavailable();
  // Not granted yet: ask once per session.
  if (state !== "granted" && last) return { kind: "skipped" };

  const { coords, error } = await position();

  if (error) {
    // A timeout or a lost signal is retried on the next visit; a refusal is remembered.
    return error.code === PERMISSION_DENIED
      ? unavailable()
      : { kind: "skipped" };
  }

  const here = { lat: coarse(coords.latitude), lng: coarse(coords.longitude) };
  if (typeof last?.lat === "number" && metres(last, here) < MOVED_M) {
    return { kind: "skipped" };
  }

  write(KEY, here);
  return { kind: "located", ...here };
}

export function dismissAddressNote() {
  write(NOTE_KEY, true);
}
