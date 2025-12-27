#!/usr/bin/env bash
# =============================================================================
# Schema Destructive Change Test (Type Removal)
# =============================================================================
#
# このスクリプトは破壊的スキーマ変更（型の削除）をテストします：
# 1. Phase 1: 全ファクト種別（InsightExtracted, ReportGenerated）でデータを保存
# 2. Types.hsからReportGeneratedをコメントアウト
# 3. Phase 2: 型削除後の読み取り挙動を検証
# 4. Types.hsを元に戻す
#
# 使用方法: ./scripts/test-schema-destructive.sh
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BACKEND_DIR="$PROJECT_ROOT/backend"
TYPES_FILE="$BACKEND_DIR/src/Domain/Types.hs"
TEST_DB_PATH="/tmp/m36-schema-destructive-test"

# 色付き出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_phase() { echo -e "${CYAN}[PHASE]${NC} $1"; }

# クリーンアップ
cleanup() {
    log_info "Cleaning up..."
    # Types.hsを元に戻す（バックアップがあれば）
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
log_info "Schema Destructive Change Test"
log_info "=========================================="
log_info ""
log_info "This test verifies what happens when a constructor"
log_info "is REMOVED from FactPayload ADT."
log_info ""

# テストDBを削除してクリーンな状態から開始
rm -rf "$TEST_DB_PATH"
log_info "Test DB path: $TEST_DB_PATH (cleaned)"

# 現在のTypes.hsをバックアップ
cp "$TYPES_FILE" "$TYPES_FILE.backup"

# =============================================================================
# Phase 1: 全ファクト種別でデータを保存（ReportGeneratedあり）
# =============================================================================

log_phase ""
log_phase "--- Phase 1: Write all fact types (with ReportGenerated) ---"

cd "$PROJECT_ROOT"

log_info "Building and running Phase 1..."
PHASE1_OUTPUT=$(nix develop --impure -c bash -c "
    cd backend
    cabal run schema-destructive-phase1 2>&1
" 2>&1 || true)

echo "$PHASE1_OUTPUT" | tail -20

if echo "$PHASE1_OUTPUT" | grep -q "SUCCESS: 3 events inserted"; then
    log_success "Phase 1: 3 events written (InsightExtracted x 2, ReportGenerated x 1)"
else
    log_error "Phase 1 failed"
    exit 1
fi

# =============================================================================
# Phase 2a: Types.hsからReportGeneratedを削除
# =============================================================================

log_phase ""
log_phase "--- Phase 2a: Remove ReportGenerated from Types.hs ---"

# Types.hsからReportGeneratedをコメントアウト（perlで置換、macOS/Linux両対応）
# 実際の行: "  | ReportGenerated Text EventId -- 新規追加：レポート生成イベント"
perl -pi -e 's/^  \| ReportGenerated/  -- | ReportGenerated/' "$TYPES_FILE"

log_info "Types.hs modified: ReportGenerated is now COMMENTED OUT"

# 確認（-- でオプション解釈終了を明示）
if grep -q -- "-- | ReportGenerated" "$TYPES_FILE"; then
    log_success "Confirmed: ReportGenerated is commented out"
else
    log_error "Failed to comment out ReportGenerated"
    exit 1
fi

# =============================================================================
# Phase 2b: 型削除後にビルドして読み取りを試みる
# =============================================================================

log_phase ""
log_phase "--- Phase 2b: Build and run with ReportGenerated REMOVED ---"

log_info "Rebuilding with modified type definition..."

# まずビルドが成功するか確認
BUILD_OUTPUT=$(nix develop --impure -c bash -c "
    cd backend
    cabal build schema-destructive-phase2 2>&1
" 2>&1 || true)

if echo "$BUILD_OUTPUT" | grep -q "Linking"; then
    log_success "Build succeeded with ReportGenerated removed"
else
    log_warn "Build output (may contain errors):"
    echo "$BUILD_OUTPUT" | tail -30
fi

log_info "Running Phase 2 (read with type removed)..."

PHASE2_OUTPUT=$(nix develop --impure -c bash -c "
    cd backend
    cabal run schema-destructive-phase2 2>&1
" 2>&1 || true)

echo ""
echo "=========================================="
echo "Phase 2 Output:"
echo "=========================================="
echo "$PHASE2_OUTPUT" | tail -40

# =============================================================================
# 結果分析
# =============================================================================

log_phase ""
log_phase "--- Result Analysis ---"

if echo "$PHASE2_OUTPUT" | grep -q "InsightExtracted: ✓ FOUND"; then
    log_success "InsightExtracted data: READABLE"
else
    log_warn "InsightExtracted data: STATUS UNKNOWN"
fi

if echo "$PHASE2_OUTPUT" | grep -q "ReportGenerated: ✓ FOUND"; then
    log_info "ReportGenerated data: STILL VISIBLE (M36 preserves type names)"
elif echo "$PHASE2_OUTPUT" | grep -q "READ ERROR"; then
    log_warn "ReportGenerated data: CAUSED READ ERROR"
elif echo "$PHASE2_OUTPUT" | grep -q "ReportGenerated: ✗ NOT FOUND"; then
    log_info "ReportGenerated data: NOT VISIBLE (filtered or failed to deserialize)"
fi

# =============================================================================
# 完了
# =============================================================================

log_info ""
log_info "=========================================="
log_success "Schema Destructive Change Test Complete"
log_info "=========================================="
log_info ""
log_info "Summary:"
log_info "  - Phase 1: Wrote InsightExtracted x 2, ReportGenerated x 1"
log_info "  - Phase 2: Read with ReportGenerated REMOVED from type definition"
log_info ""
log_info "The output above shows how M36 handles data when its"
log_info "corresponding Haskell constructor is removed."
