<!-- scripths: 0.5.0.0 -->

# Compiled Cells

Cells are normally interpreted at the GHCi prompt. Adding a `-- compile`
directive to a cell turns it into a compiled cell: its declarations are
placed in a generated module that GHC compiles to native object code, so
the definitions run at full compiled speed instead of being interpreted.

Compiled cells may only contain module-top-level content — imports,
declarations, pragmas, Template Haskell splices, and `:set -XExtension`
lines. Expressions and monadic binds (`x <- …`) belong in interpreted
cells, which can freely use anything a compiled cell defines.

Here is a function that is much faster compiled than interpreted:

```haskell
-- compile
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

collatzLength :: Int -> Int
collatzLength = go 0
  where
    go !steps 1 = steps
    go !steps n
        | even n = go (steps + 1) (n `div` 2)
        | otherwise = go (steps + 1) (3 * n + 1)
```

Compiler flags work the same way they would in an ordinary module: an
`OPTIONS_GHC` pragma in a compiled cell is hoisted to the top of the
generated module, so the `-O2` above applies to everything the module
defines. To set flags for the whole notebook instead, use a
`-- cabal: ghc-options: -O2` directive, which lands in the `ghc-options:`
field of the generated cabal project — per-cell pragmas layer on top.

This ordinary interpreted cell calls the compiled definition:

```haskell
maximum [collatzLength n | n <- [1 .. 100000]]
```

You can also name the generated module explicitly with
`-- compile: Some.Module`. Cells sharing a name contribute to the same
module, which is handy for grouping related definitions:

```haskell
-- compile: Stats

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
```

```haskell
-- compile: Stats

variance :: [Double] -> Double
variance xs = mean [(x - m) ^ (2 :: Int) | x <- xs]
  where
    m = mean xs
```

```haskell
variance [1, 2, 3, 4, 5]
```

Because the notebook is reactive, editing a compiled cell recompiles its
module and reruns the interpreted cells that depend on it — try changing
`variance` to return the standard deviation and watch the result update.
