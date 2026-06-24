# Cambiatus Frontend Deployment

The production frontend is a **static Vite bundle served by NGINX off the EC2 box**
(`app.cambiatus.io`) — **not Netlify** (a `netlify.toml` exists but is not the prod
path). The bundle is built in CI and shipped as a tarball; the box no longer builds.

## Deploy

```bash
gh workflow run "Build static frontend"          # or push a v* tag
gh run watch                                      # wait for green
gh run download -n frontend-static                # -> frontend-static.tar.gz
./deploy.sh app.cambiatus.io frontend-static.tar.gz
```

`deploy.sh` uploads the tarball, overlays the server's prod `env-config.js`, and
atomically swaps it into the nginx-served directory. No build, no nginx reload.

## Runtime config (env-config.js)

The bundle is **environment-agnostic**. API/chain URLs come from `env-config.js`
(`window._env_`, loaded at runtime by `index.html`). The repo's `public/env-config.js`
holds dev values (localhost). The **prod** values live on the server at
`/opt/cambiatus/frontend/env-config.production.js` (like the backend's `.env.production`),
and `deploy.sh` overlays it onto the bundle on every deploy. Edit prod URLs there, not
in the repo.

## Layout on the server

| Thing | Value |
|---|---|
| nginx root | `/home/ubuntu/apps/frontend/build-vite` (`sites-enabled/default`) |
| prod runtime config | `/opt/cambiatus/frontend/env-config.production.js` |
| bundle backups | `…/build-vite.bak-<ts>` (deploy keeps 3) |
| served size | ~17M static (was a 950M dev checkout when building on the box) |

## Rollback

`deploy.sh` prints the exact command. Generally:

```bash
ssh ubuntu@app.cambiatus.io 'rsync -a --delete /home/ubuntu/apps/frontend/build-vite.bak-<ts>/ /home/ubuntu/apps/frontend/build-vite/'
```

## Notes

- The old on-box build flow (git checkout + `yarn build` in `/home/ubuntu/apps/frontend`)
  is superseded. That checkout can be slimmed/removed once this flow is trusted — only
  `build-vite/` (the served output) and the nginx config need to remain.
- elm-book is a separate `elm-book/` build, deployed independently.
