# Cambiatus frontend

This project uses [Vite](https://vitejs.dev/) as the build tool and [Elm](https://elm-lang.org/) as the main language.

You must have yarn installed to develop or build.


## Local development with backend

To run the frontend against a local Phoenix backend (instead of staging):

**Prerequisites:** the backend repo must be cloned, migrated, and seeded — see its README.

**Step 1** — install dependencies:
```
yarn install
```

**Step 2** — start the backend (in the backend repo):
```
mix phx.server
```
The backend listens on `http://localhost:4000`.

**Step 3** — start the frontend:
```
USE_SUBDOMAIN=false yarn start
```
The `USE_SUBDOMAIN=false` flag switches auth storage from cookies (subdomain-scoped) to localStorage and enables `public/env-config.js`, which points the app at `localhost:4000`.

Open `http://localhost:3000` in your browser.

### Subdomain-based local dev (advanced)

Modern browsers resolve `*.localhost` to `127.0.0.1` without any `/etc/hosts` changes. You can access the app at `http://cambiatus.staging.localhost:3000` and the frontend will detect the community slug (`cambiatus`) and environment (`staging`) from the subdomain automatically. Auth tokens are stored as cookies scoped to `.staging.localhost`.


## Development (against staging backend)

```
yarn install
yarn start
```

This connects to `https://staging.cambiatus.io` by default (no local backend needed).

To update the GraphQL Elm files, run:

```
yarn generate-graphql
```

## Code Quality

We check JS with [StandardJS](https://github.com/standard/standard) and Elm with [elm-review](https://github.com/jfmengels/elm-review)
before the `git push` using [husky](https://github.com/typicode/husky). You can also run `yarn standard` and `yarn review`
at any moment to make sure your code is passing the linters.

You can also use [`elm-analyse`](https://github.com/stil4m/elm-analyse) to get even more insights about the code.
This package is not included in the repo, you should [install it manually](https://github.com/stil4m/elm-analyse#install):

```sh
yarn global add elm-analyse

elm-analyse              # view analyse result in terminal
elm-analyse -p 3001 -s   # show extended analyse in the browser on `localhost:3001`
```


## Build

```
yarn install
yarn build
```

## Deployment

Production is a static Vite bundle **built in CI and served by nginx off the EC2 box**
(not Netlify). See [`DEPLOYMENT.md`](./DEPLOYMENT.md): `gh workflow run "Build static
frontend"` → `gh run download -n frontend-static` → `./deploy.sh app.cambiatus.io
frontend-static.tar.gz`.

## Configurations

The app has a layered configuration stack:

- `public/env-config.js` — runtime config loaded via `<script>` tag before the app starts. Sets `window._env_` with API URLs, feature flags, and keys. **In local dev this file points to `localhost:4000`.** On deployed environments (staging, demo, production) this file is replaced server-side with environment-specific values.
- `src/scripts/config.js` — build-time defaults. When `window._env_` is present (i.e. `env-config.js` loaded), these are overridden by it. When absent (no `env-config.js`), falls back to hardcoded staging URLs.
- `src/elm/Flags.elm` — Elm entrypoint for configuration. Receives the resolved config as flags from `src/index.js` and decodes it into typed values.

### Build system

The app uses Vite 2.9 with `vite-plugin-elm` for Elm compilation. Key notes:

- `@sentry/browser` is stubbed to a no-op in development (`src/scripts/sentry-stub.js`) to avoid a `var global` conflict in its pre-bundled output. Sentry is only initialised in production builds.
- Node.js polyfills (`crypto`, `buffer`, `stream`, `util`, `process`) are injected via `vite-plugin-node-polyfills` for `eosjs` compatibility.
- The legacy Webpack setup is still present under `scripts/` and `config/` and can be used via `yarn start:webpack` if needed.
