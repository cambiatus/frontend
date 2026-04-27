const noop = () => {};
const noopScope = { setUser: noop, setTag: noop };

export const init = noop;
export const setTag = noop;
export const addBreadcrumb = noop;
export const captureMessage = noop;
export const withScope = (cb) => cb(noopScope);
export const configureScope = (cb) => cb(noopScope);
