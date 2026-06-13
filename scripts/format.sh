#!/usr/bin/env bash
# Format all Haskell sources with fourmolu (config: fourmolu.yaml).
# Usage: scripts/format.sh [--check]
set -euo pipefail
cd "$(dirname "$0")/.."

mode=inplace
if [[ "${1:-}" == "--check" ]]; then
    mode=check
fi

echo "[format] fourmolu --mode $mode"
fourmolu --mode "$mode" src app test
