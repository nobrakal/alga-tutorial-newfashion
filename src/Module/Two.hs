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
  subs = [s0, s1]
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
  , fullAnswer = Answer "1 + 2 * 4" Comparison "(<=)" []
  , conclusion = "TODO"
  }

s1 :: SubModule
s1 = SubModule
  {abstract = T.intercalate "\n"
    ["Attention (*) (connect) is not commutative: \n"
    ," 1 * 2 /= 2 * 1\n"
    ,"But connect staturate: \n"
    ," 1 * 1 * 1 = 1 * 1\n"
    ,"1 * 1 is a self-loop over 1"
    ]
  , instruction = "Find a simpler and equivalent graph of: \"2 * 2 * 2 * 3\""
  , clue = "TODO"
  , fullAnswer = Answer "2 * 2 * 3" Comparison "(<=)" []
  , conclusion = "TODO"
  }
