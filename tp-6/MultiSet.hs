{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module MultiSet () where

import MapV1 (Map)

data MultiSet a = MS (Map a Int)
