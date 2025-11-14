#!/usr/bin/env bash
set -euo pipefail

TAG="1.25.04"
REPO="supabase/supabase"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Fetching Supabase OSS docker setup for tag ${TAG} from ${REPO}..." >&2

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "${TMP_DIR}"' EXIT

ARCHIVE_URL="https://github.com/${REPO}/archive/refs/tags/${TAG}.tar.gz"

curl -fSL \
  --retry 5 \
  --retry-delay 5 \
  --continue-at - \
  "${ARCHIVE_URL}" \
  -o "${TMP_DIR}/supabase.tar.gz"

tar -xzf "${TMP_DIR}/supabase.tar.gz" -C "${TMP_DIR}"

SRC_DIR="${TMP_DIR}/supabase-${TAG}/docker"

if [ ! -d "${SRC_DIR}" ]; then
  echo "Expected docker directory not found in archive: ${SRC_DIR}" >&2
  exit 1
fi

# Copy docker compose setup into infra/supabase
rsync -a --delete "${SRC_DIR}/" "${DEST_DIR}/"

chmod +x "${DEST_DIR}/"*.sh 2>/dev/null || true

cat <<EOF
Supabase docker compose files for tag v${TAG} have been synced into:
  ${DEST_DIR}

Typical local usage:
  cd infra/supabase
  cp .env.example .env   # then edit secrets
  docker compose up -d
EOF
