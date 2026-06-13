#!/usr/bin/env bash
# HLint over all Haskell sources. Usage: scripts/lint.sh [--fix]
set -euo pipefail
cd "$(dirname "$0")/.."

if [[ "${1:-}" == "--fix" ]]; then
    hlint src app test --refactor --refactor-options=-i
fi
hlint src app test
