**(Under construction)**

## Code style

**(Under construction)**

TODO(typesafety): Get as many of these as possible to be applied by the
formatter/linter

[Kowainik's style guide](https://kowainik.github.io/posts/2019-02-06-style-guide)
is a decent base.

### Line length

* Maximum line length for code is 120 columns.
* If possible, do not extend past 80 columns in block comments.
* If possible, do not extend past 120 columns in inline comments.

### Alignment

#### Indentation

All lines should be indented by a number of columns that is a multiple of 4,
except for `where` keywords which should be put on its own line and be indented
by 2 less than the block it defines.

```haskell
func :: List Int -> IO Int
func xs = do
    y <- some xs
    stuff y
  where
    some :: List Int -> IO Int
    some = undefined

    stuff :: Int -> IO Int
    stuff = undefined
```

#### Long function signatures

When splitting long function signatures, put separators after types rather than
before them (makes code more `grep`-able ).

Example:

```haskell
myFunction ::
    (Show a, Eq a) =>
    Int ->
    List a ->
    IO Unit
```

### Module header

Use the following order, each separated by a blank line:

* Module docstring
* Module-level pragmas (for example, `LANGUAGE` and GHC options)
* Module declaration and export list
* Imports from `base`
* Imports from Hackage dependencies
* imports from local modules

Leave two blank lines after the imports.

Example:

```haskell
-- | Module docstring.

{-# LANGUAGE DeriveGeneric #-}

module Shaunlib (
    SomeType,
    someFunction,
) where

import Control.Concurrent (forkIO)
import Data.Text qualified as Text

import Data.Aeson qualified as Aeson

import Shaunlib.Internal.Utils (putTxtLn)


myFunc :: a -> a
myFunc = id
```
