-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}

module Main where

import Data.Tuple.Experimental (Unit)

import Shaunlib.Internal.Gateway

main :: IO Unit
main = startGateway
