# scripths

GHCi scripts for standalone execution and Markdown documentation.

**scripths** lets you write `.ghci` scripts with dependency management and run them as standalone programs — or embed executable Haskell code blocks in Markdown files and evaluate them notebook-style with captured output.

## Features

- **Standalone `.ghci` execution** — Run GHCi scripts directly from the command line, with automatic dependency.
- **Cabal metadata directives** — Declare `build-depends`, `default-extensions`, and `ghc-options` inline using `-- cabal:` comments.
- **Markdown notebooks** — Execute Haskell code blocks inside Markdown files and render the output back into the document as block quotes.
- **Smart GHCi rendering** — Multi-line definitions are automatically wrapped in `:{`/`:}` blocks, and IO binds / Template Haskell splices are handled correctly as individual statements.

## Installation

```bash
cabal install scripths
```

## CLI Usage

```
scripths [-o FILE | --output=FILE] [-p DIR | --package DIR]... [--no-local-project] <script>
```

When `-o` / `--output` is provided for Markdown files, the result is written to that path. Otherwise it is printed to stdout. The file extension determines the mode. `.ghci` / `hs` files are parsed and executed as a standalone GHCi script. `.md` / `.markdown` files are processed as a notebook with captured output.

Requires GHC and cabal-install on your PATH.

## Quick Start

### Running a GHCi script

Create a file `example.ghci`:

```haskell
double :: Int -> Int
double = (*2)

-- cabal: build-depends: text
:set -XOverloadedStrings
import qualified Data.Text as T

T.take 10 "hello"

double 5
```

Run it:

```bash
scripths example.ghci
```

### Running a Markdown notebook

Create a file `notebook.md`:

````markdown
# My Analysis

Some introductory prose.

```haskell
print (5 + 5)
```

Define some values:

```haskell
x = 10
y = 20
```

We can then add the values in the next block and
the output is embeded below.

```haskell
x + y
```
````

Run it and write the results to a new file:

```bash
scripths -o output.md notebook.md
```

Each Haskell code block is evaluated in order, and its output is inserted into the Markdown as a block quote beneath the code fence.

## Cabal Metadata Directives

You can declare dependencies, language extensions, and GHC options directly inside your scripts using `-- cabal:` comments:

```
-- cabal: build-depends: text, containers
-- cabal: default-extensions: OverloadedStrings, TypeApplications
-- cabal: ghc-options: -Wall
```

## Local packages

By default `build-depends` resolve from Hackage. To build a script or notebook
against a **local checkout** instead — e.g. to document a library version that
only exists on a branch — there are two mechanisms.

### The enclosing project (zero-config)

If a script/notebook lives inside a cabal project and a `build-depends` names that
project's package (or one of its sub-libraries, `pkg:sublib`), scripths builds
against the local working tree automatically — so documenting your own repo just
works:

```bash
scripths -o docs/MANUAL.md docs/base/MANUAL.md
```

Pass `--no-local-project` to suppress this and resolve every `build-depends` from
Hackage instead — e.g. to verify the doc's examples against the *released*
version of your package rather than the working tree. Explicit `--package` dirs
are unaffected.

### `--package DIR`

Point at another local checkout (a sibling package, or a sub-library project).
`DIR` must be a package root (contain a `.cabal`); it is resolved to an absolute
path at the invocation site, so nothing machine-specific is committed to the
shared document. Repeatable.

```bash
scripths --package ../granite -o out.md in.md
```

### Pinned git packages (`source-repository-package`)

To pin a **pushed** commit/tag reproducibly (and commit the pin into the shared
document), declare a `source-repository-package` directive as
`<location> <ref> [subdir]`:

```
-- cabal: source-repository-package: https://github.com/owner/repo v1.2.3
-- cabal: source-repository-package: https://github.com/owner/mono abc123 sub/dir
```

scripths writes these as `source-repository-package` stanzas in the generated
`cabal.project`. Use a local checkout (above) for an **unpushed** branch, and a git
pin for a **pushed**, reproducible reference.
