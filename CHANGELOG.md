# Revision history for scripths

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
