# Contributing to scripths

## Project Overview

**scripths** runs Haskell scripts and Markdown notebooks through GHCi: it
parses `-- cabal:` / `-- compile` directives, resolves dependencies into a
throwaway cabal project, renders cells to GHCi input (or to generated
modules), and captures per-cell output via nonce markers. Sabela
(`../sabela`) builds on this library for its reactive notebook server.

## Commands

```bash
cabal build                  # Library + scripths executable
cabal test                   # tasty suite (test/Test/*.hs)
./scripts/format.sh          # fourmolu over src/, app/, test/
./scripts/format.sh --check  # Dry-run; non-zero exit on drift
./scripts/lint.sh            # HLint over src/, app/, test/
```

Run format and lint before tests, and again before submitting a PR.

## Style guide

- **Formatter**: fourmolu, 4-space indent, 80-column limit, leading commas —
  the committed `fourmolu.yaml` is identical to sabela's. Never hand-format
  against it.
- **Module size: keep every module ≤ 300 lines.** Split an oversized module
  into focused submodules instead of letting it grow. (`ScriptHs.Render` and
  `ScriptHs.Run` predate this cap and still exceed it — shrink them when
  touched substantially; do not let new modules join them.)
- **Comments are top-level and ≤ 3 lines** (haddock or a block above the
  declaration), unless they contain a code example. No inline narrative
  comments threaded through expressions.
- HLint-clean: `./scripts/lint.sh` should report no hints.

