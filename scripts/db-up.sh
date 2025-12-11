#!/usr/bin/env bash
set -e

# Define data directory relative to the script location (assuming scripts/db-up.sh)
# Logic: script is in scripts/, so parent is root.
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export PG16_DATA="$PROJECT_ROOT/database/.data"

# Check if PG16_BIN is set (by flake), if not, try to find it or fail
if [ -z "$PG16_BIN" ]; then
  echo "PG16_BIN is not set. Please run this inside the nix devShell."
  # Optional fallback if using global install
  # export PG16_BIN=$(dirname $(which postgres))
  exit 1
fi

echo "Starting Postgres 16..."
echo "Data Dir: $PG16_DATA"
echo "Bin Dir:  $PG16_BIN"

process-compose -f "$PROJECT_ROOT/database/process-compose.yaml" up
