#!/bin/bash
set -euo pipefail

# Cambiatus frontend deploy — ship a PREBUILT static bundle to the server.
#
# The bundle is built in CI (.github/workflows/build-static.yml) so prod never
# builds on the box. This script does NOT build — it uploads the artifact, overlays
# the server's prod runtime config (env-config.js), and atomically swaps it into the
# nginx-served directory.
#
# Usage:
#   gh run download -n frontend-static            # -> frontend-static.tar.gz
#   ./deploy.sh app.cambiatus.io frontend-static.tar.gz
#
# See DEPLOYMENT.md.

DEPLOY_USER="ubuntu"
SERVE_PATH="/home/ubuntu/apps/frontend/build-vite"   # nginx root (sites-enabled/default)
ENV_CONFIG="/opt/cambiatus/frontend/env-config.production.js"  # prod runtime config (on box)

SERVER="${1:-}"
ARTIFACT="${2:-}"

if [ -z "$SERVER" ] || [ -z "$ARTIFACT" ]; then
  echo "Usage: ./deploy.sh <server> <frontend-static.tar.gz>"
  echo "Example: ./deploy.sh app.cambiatus.io frontend-static.tar.gz"
  exit 1
fi
if [ ! -f "$ARTIFACT" ]; then
  echo "Error: artifact '$ARTIFACT' not found. Download it first:"
  echo "  gh run download -n frontend-static"
  exit 1
fi

# Refuse to deploy while the prod maintenance page is on (DB windows, box reboots).
# The flag file is the one nginx checks (scripts/box-runbook.md §10). ssh exits 0 =
# flag present, 1 = absent, 255 = ssh itself failed; on 255 we carry on and let the
# scp below fail exactly as it did before this check existed.
MAINTENANCE_FLAG="/home/ubuntu/apps/maintenance/on"
flag_rc=0
ssh "${DEPLOY_USER}@${SERVER}" "test -e ${MAINTENANCE_FLAG}" || flag_rc=$?
if [ "$flag_rc" -eq 0 ]; then
  echo "Error: maintenance window in progress on ${SERVER} (${MAINTENANCE_FLAG} exists)."
  echo "  A DB window or box reboot is running; deploying now can restart against a down"
  echo "  database or race the reboot. See scripts/box-runbook.md §10 and rerun when it is off."
  exit 1
elif [ "$flag_rc" -ne 1 ]; then
  echo "Warning: could not check the maintenance flag (ssh exit ${flag_rc}); continuing."
fi

TS="$(date +%Y%m%d_%H%M%S)"
REMOTE_TGZ="/tmp/frontend-static-${TS}.tar.gz"

echo "→ Uploading $ARTIFACT to ${SERVER}:${REMOTE_TGZ}"
scp "$ARTIFACT" "${DEPLOY_USER}@${SERVER}:${REMOTE_TGZ}"

echo "→ Deploying on ${SERVER}"
ssh "${DEPLOY_USER}@${SERVER}" "bash -s" -- "$REMOTE_TGZ" "$SERVE_PATH" "$ENV_CONFIG" "$TS" <<'REMOTE'
set -euo pipefail
REMOTE_TGZ="$1"; SERVE_PATH="$2"; ENV_CONFIG="$3"; TS="$4"

STAGING="${SERVE_PATH}.staging-${TS}"
echo "  extracting bundle to ${STAGING}"
rm -rf "$STAGING"; mkdir -p "$STAGING"
tar -xzf "$REMOTE_TGZ" -C "$STAGING"
test -f "${STAGING}/index.html" || { echo "  ERROR: index.html missing in artifact"; rm -rf "$STAGING"; exit 1; }

echo "  overlaying prod runtime config (env-config.js)"
test -f "$ENV_CONFIG" || { echo "  ERROR: prod env-config not found at $ENV_CONFIG"; rm -rf "$STAGING"; exit 1; }
cp "$ENV_CONFIG" "${STAGING}/env-config.js"

if [ -d "$SERVE_PATH" ]; then
  echo "  backing up current bundle -> $(basename "$SERVE_PATH").bak-${TS}"
  cp -al "$SERVE_PATH" "${SERVE_PATH}.bak-${TS}"   # cheap hardlink snapshot
fi

echo "  swapping bundle into place (rsync --delete)"
rsync -a --delete "${STAGING}/" "${SERVE_PATH}/"
rm -rf "$STAGING" "$REMOTE_TGZ"

# Prune old bundle backups — keep the 3 most recent.
ls -dt "${SERVE_PATH}".bak-* 2>/dev/null | tail -n +4 | xargs -r rm -rf
echo "  done (static — no nginx reload needed)"
REMOTE

echo "→ Verifying"
ssh "${DEPLOY_USER}@${SERVER}" "curl -s -o /dev/null -w 'index http_code=%{http_code}\n' http://localhost/ -H 'Host: muda.cambiatus.io'; \
  echo -n 'env-config API_URL: '; grep -o 'API_URL[^,]*' ${SERVE_PATH}/env-config.js | head -1"

echo "✓ Deployed. Rollback: ssh ${DEPLOY_USER}@${SERVER} 'rsync -a --delete ${SERVE_PATH}.bak-${TS}/ ${SERVE_PATH}/'"
