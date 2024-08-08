-- | Random utility functions (that should probably exist in the Prelude)

-- TODO(typesafety): Tighten up export list
module Shaunlib.Internal.Utils where

import Data.Tuple.Experimental (Unit)

import Data.Text (Text)
import Data.Text.IO qualified
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pShow)


{-# WARNING todo "todo left in code" #-}
todo :: a
todo = undefined

putTxtLn :: Text -> IO Unit
putTxtLn = Data.Text.IO.putStrLn

pShowTxt :: Show a => a -> Text
pShowTxt = toStrict . pShow
