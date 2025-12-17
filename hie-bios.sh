#!/usr/bin/env bash
# HLS用のbiosスクリプト
# 引数: 対象ファイルのパス

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GHC_WITH_PKGS="/nix/store/kgkjh0b4mycaf8j1vhg1vhgc8wh3xpsh-ghc-9.6.7-with-packages"
GHC_PKG_DB="$GHC_WITH_PKGS/lib/ghc-9.6.7/lib/package.conf.d"

TARGET_FILE="$1"

# デバッグログ
echo "=== hie-bios.sh debug ===" >> /tmp/hie-bios.log
echo "SCRIPT_DIR: $SCRIPT_DIR" >> /tmp/hie-bios.log
echo "TARGET_FILE: $TARGET_FILE" >> /tmp/hie-bios.log

if [ ! -d "$GHC_PKG_DB" ]; then
  echo "Error: Package DB not found" >&2
  exit 1
fi

# GHCオプションを出力（絶対パス）
cat <<EOF
-clear-package-db
-global-package-db
-package-db
$GHC_PKG_DB
-i$SCRIPT_DIR/backend/src
-i$SCRIPT_DIR/backend/app
-i$SCRIPT_DIR/backend/test
$TARGET_FILE
EOF
