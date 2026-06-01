# Design: non-cabal / Backpack / custom-prelude support

Status: **proposed** (design only — no code yet)

How to make `scripths` work for projects that are not driven by `cabal` and/or
that use Backpack mix-ins and a replaced (custom) `Prelude`.

## Problem

`scripths` is hardwired to one strategy: **synthesize a throwaway cabal package
under `~/.scripths/<name>/` and run `cabal repl exe:main`**, feeding it a
generated `-ghci-script`. Two parts of that strategy block non-cabal, Backpack,
and custom-prelude projects:

1. **The driver is hardcoded to `cabal`.** `cabalArgs` (`src/ScriptHs/Run.hs:341`)
   and `proc "cabal"` (`src/ScriptHs/Run.hs:362`, `src/ScriptHs/Run.hs:377`)
   bake in `cabal repl exe:main`. Stack, a bare `ghci` against a package-db, and
   rules_haskell-produced environments can't be reached at all.

2. **The synthetic `.cabal` can't express Backpack or a replaced `Prelude`.**
   `renderCabalFile` (`src/ScriptHs/Run.hs:319`) always emits `base` plus a plain
   `executable main` with `build-depends` / `default-extensions` / `ghc-options`
   and nothing else. There is no `mixins:`, no `signatures:`, no
   `NoImplicitPrelude`. A custom prelude is normally wired as
   `mixins: rio (RIO.Prelude as Prelude)` or `NoImplicitPrelude` + an explicit
   import — none of which `scripths` can emit, and `CabalMeta`
   (`src/ScriptHs/Parser.hs:53`) has no fields to carry it.

A smaller landmine: the internal cd shims lean on `Prelude.>>` / `Prelude.pure`
(`compileCdTo`, `src/ScriptHs/Run.hs:206`) and qualified `System.Directory`
(`cdDirective`, `src/ScriptHs/Run.hs:165`). Those qualified references survive
`NoImplicitPrelude`, but a Backpack mix-in that *hides* `Prelude` would break
them.

## Key insight

The cleanest fix is not "teach the synthesizer to emit Backpack stanzas" — it is
to **stop synthesizing the environment and attach to the project's real
component instead.** The project's own `.cabal` / `hpack` / Backpack wiring
already configured the mix-ins, the signature instantiation, and the custom
`Prelude`. If `scripths` feeds its generated `-ghci-script` into *that*
component's repl, all of it comes for free.

The work splits into three independent levers.

---

## Lever 1 — Pluggable REPL backend

Decouples `scripths` from `cabal`. The only contract a backend must satisfy:
*give me a non-interactive GHCi that honors a `-ghci-script` file.*

```haskell
data ReplBackend
  = Cabal                 -- cabal repl <target> --repl-option=-ghci-script=…
  | Stack                 -- stack repl <target> --ghci-options "-ghci-script=…"
  | Ghci                  -- ghci -ghci-script=…  (against a ghc.environment / package-db)
  | Custom [String]       -- user argv template with a {ghci-script} placeholder
```

**Wiring.** `cabalArgs` (`src/ScriptHs/Run.hs:341`) becomes
`replInvocation :: ReplBackend -> Target -> FilePath -> (String, [String])`,
returning the executable and args. `runGhc` / `captureGhc`
(`src/ScriptHs/Run.hs:374`, `src/ScriptHs/Run.hs:355`) call it instead of
hardcoding `proc "cabal"`. `RunOptions` (`src/ScriptHs/Run.hs:39`) gains
`roBackend :: ReplBackend` and `roTarget :: Target`.

**Selection precedence:** `--backend` flag › `-- cabal: repl-command:` directive ›
autodetect (`stack.yaml` ⇒ `Stack`, `cabal.project` / `*.cabal` ⇒ `Cabal`, else
error asking for `--backend`).

**Nix / Bazel** are covered transparently: inside a `nix develop` shell the
existing `Cabal` / `Ghci` path already works because `cabal` / `ghc` are on
PATH; `Ghci` against a generated `ghc.environment.*` covers the no-cabal case.

**New CLI surface** in `app/Main.hs`: `--backend cabal|stack|ghci|custom`, and
`--repl-command "<argv with {ghci-script}>"`.

---

## Lever 2 — Attach-to-component mode (explicit)

This is the lever that makes Backpack + custom preludes work, by **not
synthesizing anything** and instead running the script's GHCi lines inside the
project's real component.

**Trigger — explicit only.** `--component <spec>` flag or
`-- cabal: component: <spec>`, where `<spec>` is whatever the backend
understands (`lib:foo`, `foo:lib:internal`, `exe:bar` for cabal; `foo:lib` for
stack). No autodetection, no walking up the tree.

**Behaviour.** When a component is given, `ensureProject`
(`src/ScriptHs/Run.hs:93`) takes a different branch:

- Skip `Main.hs`, the synthetic `.cabal`, and `cabal.project` generation
  entirely.
- Still generate `script.ghci` (the `autoPrintDirective` + cd shim +
  `toGhciScript` body) into a temp location.
- The repl target becomes the named component, run with `cwd` set to the project
  root rather than `~/.scripths/<name>`.

Because the component's own build config supplies the Backpack `mixins:` /
`signatures:` / instantiation and the replaced `Prelude`, the script sees
exactly the project's environment — internal modules importable, custom prelude
in scope, no synthesis fighting it.

**Hardening required for custom preludes.** Bind the handful of names `scripths`
needs (`>>`, `pure`, `setCurrentDirectory`, the TH `runIO`) under
`ScripthsInternal*` qualified imports and stop referencing `Prelude.` by name,
so a Backpack mix-in that *hides* `Prelude` doesn't break the cd shim
(`src/ScriptHs/Run.hs:165`, `src/ScriptHs/Run.hs:206`). `autoPrintDirective`'s
`Show a` / `print` / `putStr` are re-exported by relude / rio / protolude, so
the auto-printer is fine as-is — but worth a test against a prelude that doesn't
re-export `Show` to decide whether to guard it behind a flag.

**Interaction with existing local-package logic.** Attach mode and the current
`enclosingProjectFor` / `--package` path are mutually exclusive. When
`--component` is set, the `packages:` / `build-depends`-injection machinery
(`src/ScriptHs/Run.hs:100`–`src/ScriptHs/Run.hs:125`) is bypassed, since the real
build owns dependency resolution. `--no-local-project` becomes a no-op in this
mode.

---

## Lever 3 — Directive passthrough for standalone scripts (independent)

For one-off scripts with no project to attach to (someone wants relude in a
`.ghci`), extend the synth model rather than replace it:

- Add `metaMixins`, `metaSignatures` to `CabalMeta`
  (`src/ScriptHs/Parser.hs:53`) and parse `-- cabal: mixins:` /
  `-- cabal: signatures:` in `parseCabalMeta` (`src/ScriptHs/Parser.hs:152`).
- Emit them from `renderCabalFile` (`src/ScriptHs/Run.hs:319`) and the
  single-file header `renderCabalScriptHeader` (`src/ScriptHs/Render.hs:492`).
- Convenience `-- cabal: prelude: RIO.Prelude (rio)` expands to
  `NoImplicitPrelude` + `mixins: rio (RIO.Prelude as Prelude)` + the `rio` dep.

cabal handles Backpack natively, so the synth package can genuinely instantiate
a custom prelude. Orthogonal to Levers 1–2; can ship on its own.

---

## Open risks to settle before building

- **`Custom` / `Ghci` backends and the TH cwd shim.** The compile-time chdir
  (`usesTemplateHaskell` → `compileCdTo`) assumes the repl recompiles in-process
  the way `cabal repl` does. A bare `ghci` against a frozen package-db behaves
  the same, but a custom wrapper that pre-builds could resolve `./relative` TH
  paths differently — needs a test per backend.
- **Capture ordering.** `captureGhc` merges stdout+stderr for inline errors
  (`src/ScriptHs/Run.hs:355`); this must be re-verified per backend. `stack` and
  wrapper scripts can emit progress chatter on stderr that would pollute notebook
  output — likely need a `-v0`-equivalent per backend or a marker-based filter.
- **No component autodetection** (deliberate). When someone runs `scripths`
  inside a Backpack project *without* `--component`, the error should be
  actively helpful: detect that the enclosing package has `signatures:` /
  `mixins:` and suggest the flag.

## Recommended build order

**Lever 1 → Lever 2 → Lever 3.** Lever 2 depends on Lever 1's backend
abstraction; Lever 3 is standalone. Per repo workflow, each lever is built
tests-first (TDD).
