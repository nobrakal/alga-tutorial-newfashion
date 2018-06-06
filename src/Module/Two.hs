{-# LANGUAGE OverloadedStrings #-}

module Module.Two
  (mod2)
where

import Types
import qualified Data.Text as T

mod2 :: Module
mod2 = Module
  {
  name = "Mathematical laws",
  desc = "If there is an algebra, then there is mathematicals laws",
  subs = [s0]
  }

s0 :: SubModule
s0 = SubModule
  {abstract = T.intercalate "\n"
    ["We already view that (+) (overlay) is commutative: \n"
    ," 1 + 2 = 2 + 1\n"
    ,"It is also idempotent: \n"
    ," 1 + 1 = 1\n"
    ]
  , instruction = "Find a simpler and equivalent graph of: \"1 + 2 + 2 + 1 * 4\""
  , clue = "TODO"
  , fullAnswer = Answer "1 + 2 + 3 * 4" Comparison "(<=)"
  , conclusion = "TODO"
  }
