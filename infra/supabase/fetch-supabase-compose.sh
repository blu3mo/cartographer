#!/usr/bin/env bash
set -euo pipefail

TAG="1.25.04"
REPO="supabase/supabase"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="${SCRIPT_DIR}/bundle"

echo "Fetching Supabase OSS docker setup for tag ${TAG} from ${REPO}..." >&2
echo "Output directory: ${OUT_DIR}" >&2

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "${TMP_DIR}"' EXIT

ARCHIVE_URL="https://github.com/${REPO}/archive/refs/tags/${TAG}.tar.gz"

# Large (~1GB) download; retry and resume on failure
curl -fSL \
  --retry 5 \
  --retry-delay 5 \
  --continue-at - \
  "${ARCHIVE_URL}" \
  -o "${TMP_DIR}/supabase.tar.gz"

# Extract only the docker directory
cd "${TMP_DIR}"
tar -xzf "supabase.tar.gz"

SRC_DIR="${TMP_DIR}/supabase-${TAG}/docker"

if [ ! -d "${SRC_DIR}" ]; then
  echo "Expected docker directory not found in archive: ${SRC_DIR}" >&2
  exit 1
fi

# Copy docker compose setup into dedicated bundle directory
mkdir -p "${OUT_DIR}"
rsync -a --delete "${SRC_DIR}/" "${OUT_DIR}/"

cat <<EOF
Supabase docker compose files for tag v${TAG} have been synced into:
  ${OUT_DIR}

Typical local usage:
  cd infra/supabase/bundle
  cp .env.example .env   # then edit secrets
  docker compose up -d
EOF
