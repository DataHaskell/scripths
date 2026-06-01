# Revision history for scripths


## 0.4.1.0 -- 2056-05-31
* **Custom-prelude support**: auto print (our hook to rout evething but strings to shw)
  now doesn't assume a prelude.
* **Cleaner error rendering** (notebooks): don't leak internals when printing errors.
* **Version tag**: files may carry a first-line tag recording the scripths that
  wrote them — `-- scripths: X` in `.ghci`/`.hs` scripts, `<!-- scripths: X -->`
  in `.md`/`.markdown` notebooks. Running a file that declares a *newer* scripths
  than the binary prints a warning and continues. scripths stamps/refreshes the
  tag when it writes a notebook (`-o`/`--in-place`); stdout output is left
  unstamped. The tag is recognised after a leading `#!` shebang (scripts) or YAML
  frontmatter block (notebooks), tolerating a BOM, CRLF, and indentation.
  Behaviour change: the first `--in-place` run on an existing notebook inserts the
  `<!-- scripths: X -->` line (and normalises leading blank lines), a one-time
  visible diff in tracked files; it is invisible when rendered and idempotent
  thereafter.
* **`scripths --version` / `-v`** prints the scripths version.

## 0.4.0.1 -- 2026-05-30
* Parse pandoc markdown code fences as haskel code fences.

## 0.4.0.0 -- 2026-05-29

* **`OverloadedStrings` on by default** in every scripths repl, so string literals
  work directly as `Text` / `ByteString`.
* **Compile-time Template Haskell now sees your working directory**: notebooks
  using TH (e.g. `$(declareTable "./data/x.db" …)`) have their body bracketed with
  a compile-time `setCurrentDirectory`, so splices that read files at compile time
  resolve `./relative` paths against your tree instead of the throwaway project
  dir. Only added when the notebook actually uses TH.
* **In-place notebooks**: `-i` / `--in-place` rewrites a notebook over itself,
  stripping and replacing previously rendered output. `reassemble` is now
  idempotent — re-running no longer accumulates blank lines around code fences.
* **Inline block errors**: a code block that fails to compile now renders its GHC
  diagnostic beneath the block (stdout and stderr are captured as one ordered
  stream) instead of producing silent empty output.
* **Second local package**: new `-- cabal: packages: <dir>, …` directive for
  depending on non-enclosing local packages (relative to the script). Local
  package names — from `packages:`, `--package`, or the enclosing project — are
  now added to the synthetic `build-depends` automatically, so their modules
  import without a separate `build-depends` line.
* **Output marker renamed** from `sabela:mime` to `scripths:mime`. The legacy
  marker is still recognised when re-parsing older notebooks.

## 0.3.3.0 -- 2026-05-24

* Build against **local packages**: auto-include the enclosing cabal project when a
  `build-depends` names it (zero-config; `--no-local-project` to disable), and a
  repeatable `--package DIR` / `-p DIR` flag for sibling/sub-library checkouts. Lets
  documentation build against an unreleased local working tree.
* New `-- cabal: source-repository-package: <location> <ref> [subdir]` directive for
  pinning a pushed git commit/tag reproducibly; rendered into the generated
  `cabal.project`.
* `CabalMeta` gains `metaSourceRepos` and `metaUnknownKeys`; unrecognised `-- cabal:`
  keys now warn to stderr (instead of being silently dropped).
* The generated `cabal.project` is regenerated each run (guarded by a `-- managed by
  scripths` sentinel so hand-edited project files are preserved), replacing the old
  write-once behaviour. Rewrote the CLI argument parser.

## 0.3.1.0 -- 2026-04-23

* Better block parsing when there are function signatures and comments.

## 0.3.0.1 -- 2026-03-14

* improve block parsing

## 0.2.0.2 -- 2026-03-14

* Stop adding extra white space to code cells.

## 0.2.0.1 -- 2026-03-01

* Fix issue with SVG mime type.

## 0.2.0.0 -- 2026-03-01

* Add mime type to output.

* Make blocks out of single lines of code as well.

## 0.1.0.2 -- 2026-02-28

* Make blocks out of single lines of code as well.

## 0.1.0.1 -- 2026-02-24

* Fix code new ine behaviour for code fencing.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
