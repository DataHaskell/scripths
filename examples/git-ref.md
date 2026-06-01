<!-- scripths: 0.4.1.0 -->

# Pinning a dependency to a git ref

scripths can build a notebook against a specific **git commit or tag** of a
dependency, via a `source-repository-package` directive. This is useful for
documenting a library at a pinned, reproducible version — without waiting for a
Hackage release.

Here we pin [`granite`](https://github.com/mchav/granite) to an exact commit
(the one tagged `v0.3.0.0`) and draw a bar chart with it:

```haskell
-- cabal: build-depends: granite, text
-- cabal: default-extensions: OverloadedStrings
-- cabal: source-repository-package: https://github.com/mchav/granite 779031d3727c35c1dd3731685018500020307560
import qualified Data.Text.IO as T
import Granite

T.putStrLn $
    bars
        [ ("Apples", 30)
        , ("Bananas", 45)
        , ("Cherries", 18)
        ]
        defPlot {plotTitle = "Fruit sales"}
```

The directive value is `<location> <ref> [subdir]`. scripths writes it as a
cabal `source-repository-package` stanza (`type: git`, `tag: <ref>`), and
cabal's `tag:` accepts a commit hash, tag, or branch name interchangeably — so
any of them work. Evaluate it with:

```sh
scripths examples/git-ref.md
```
