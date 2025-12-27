-#!/usr/bin/env bash
# =============================================================================
# Schema Evolution Integration Test
# =============================================================================
#
# このスクリプトは実際のスキーマ進化シナリオをテストします：
# 1. v1の型定義（ReportGeneratedなし）でDBにデータを書き込む
# 2. v2の型定義（ReportGeneratedあり）に変更
# 3. v2で既存DBに接続して読み書きが可能か確認
#
# 使用方法: ./scripts/test-schema-evolution.sh
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BACKEND_DIR="$PROJECT_ROOT/backend"
TYPES_FILE="$BACKEND_DIR/src/Domain/Types.hs"
TEST_DB_PATH="/tmp/m36-schema-evolution-real-test"

# 色付き出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# クリーンアップ
cleanup() {
    log_info "Cleaning up..."
    rm -rf "$TEST_DB_PATH"
    # Types.hsをv2（ReportGeneratedあり）に戻す
    if [[ -f "$TYPES_FILE.backup" ]]; then
        mv "$TYPES_FILE.backup" "$TYPES_FILE"
        log_info "Restored original Types.hs"
    fi
}
trap cleanup EXIT

# =============================================================================
# メイン処理
# =============================================================================

log_info "=========================================="
log_info "Schema Evolution Integration Test"
log_info "=========================================="

# テストDBを削除してクリーンな状態から開始
rm -rf "$TEST_DB_PATH"
log_info "Test DB path: $TEST_DB_PATH"

# 現在のTypes.hsをバックアップ
cp "$TYPES_FILE" "$TYPES_FILE.backup"

# =============================================================================
# Phase 1: v1型定義（ReportGeneratedなし）でDBにデータを書き込む
# =============================================================================

log_info ""
log_info "--- Phase 1: v1 type definition (without ReportGenerated) ---"

# Types.hsからReportGeneratedを削除（sed でコメントアウト）
# Nix環境のGNU sedを使用するため -i オプションに引数は不要
sed -i 's/| ReportGenerated Text EventId/-- | ReportGenerated Text EventId  -- REMOVED FOR SCHEMA EVOLUTION TEST/' "$TYPES_FILE"

log_info "Modified Types.hs: removed ReportGenerated"

# v1でビルド・テスト実行
log_info "Building and running Phase 1 test..."

cd "$PROJECT_ROOT"
nix develop --impure -c bash -c "
    cd backend
    cabal run schema-evolution-phase1
"

if [[ $? -eq 0 ]]; then
    log_success "Phase 1: v1 build and basic tests passed"
else
    log_error "Phase 1 failed"
    exit 1
fi

# =============================================================================
# Phase 2: v2型定義（ReportGeneratedあり）に変更して既存DBにアクセス
# =============================================================================

log_info ""
log_info "--- Phase 2: v2 type definition (with ReportGenerated) ---"

# Types.hsを元に戻す（ReportGeneratedを復活）
mv "$TYPES_FILE.backup" "$TYPES_FILE"
log_info "Restored Types.hs: ReportGenerated is back"

# v2でビルド・テスト実行
log_info "Building and running Phase 2 test..."

nix develop --impure -c bash -c "
    cd backend
    cabal run schema-evolution-phase2
"

if [[ $? -eq 0 ]]; then
    log_success "Phase 2: v2 build and tests passed"
else
    log_error "Phase 2 failed"
    exit 1
fi

# =============================================================================
# 結果
# =============================================================================

log_info ""
log_info "=========================================="
log_success "Schema Evolution Integration Test: PASSED"
log_info "=========================================="
log_info ""
log_info "Verified:"
log_info "  1. v1 (without ReportGenerated) builds and runs correctly"
log_info "  2. v2 (with ReportGenerated) builds and runs correctly"
log_info ""
log_warn "Note: This test verifies that the codebase compiles with both"
log_warn "type versions. For true schema evolution testing with persistent"
log_warn "data, additional integration tests with fixed DB paths are needed."
