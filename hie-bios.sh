#!/usr/bin/env bash
# HLS用のbiosスクリプト
# 引数: 対象ファイルのパス
# HLS biosクレードルは$HIE_BIOS_OUTPUTファイルにGHCフラグを書き込む必要がある

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GHC_WITH_PKGS="/nix/store/kgkjh0b4mycaf8j1vhg1vhgc8wh3xpsh-ghc-9.6.7-with-packages"
GHC_PKG_DB="$GHC_WITH_PKGS/lib/ghc-9.6.7/lib/package.conf.d"

TARGET_FILE="$1"

# デバッグログ
echo "=== hie-bios.sh debug ===" >> /tmp/hie-bios.log
echo "SCRIPT_DIR: $SCRIPT_DIR" >> /tmp/hie-bios.log
echo "TARGET_FILE: $TARGET_FILE" >> /tmp/hie-bios.log
echo "HIE_BIOS_OUTPUT: $HIE_BIOS_OUTPUT" >> /tmp/hie-bios.log

if [ ! -d "$GHC_PKG_DB" ]; then
  echo "Error: Package DB not found" >&2
  exit 1
fi

# HIE_BIOS_OUTPUTが設定されている場合はファイルに書き込む
# 設定されていない場合はstdoutに出力（手動テスト用）
OUTPUT_TARGET="${HIE_BIOS_OUTPUT:-/dev/stdout}"

# GHCオプションをHIE_BIOS_OUTPUTファイルに書き込む
cat > "$OUTPUT_TARGET" <<EOF
-clear-package-db
-global-package-db
-package-db
$GHC_PKG_DB
-i$SCRIPT_DIR/backend/src
-i$SCRIPT_DIR/backend/app
-i$SCRIPT_DIR/backend/test
-XDataKinds
-XDeriveAnyClass
-XDerivingStrategies
-XDerivingVia
-XDuplicateRecordFields
-XLambdaCase
-XNoFieldSelectors
-XOverloadedRecordDot
-XOverloadedStrings
-XRecordWildCards
-XTypeApplications
-XTypeFamilies
-XGHC2021
EOF

