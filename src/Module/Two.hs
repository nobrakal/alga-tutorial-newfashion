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
  subs =
    [
    SubModule
      {abstract = T.intercalate "\n"
        ["We already view that (+) (overlay) is _commutative_ : \n"
        ,">>> 1 + 2 == 2 + 1\n"
        ,"It is also associative: \n"
        ,">>> 1 + (2 + 3) == (1 + 2) + 3\n"
        ,"In other words, the order in which you are overlaying graphs is _not_ important"
        ]
      , instruction = "Find an equivalent graph of: \"((1 + 2) + 3) * 4\""
      , clue = "Try to use the law of associativity"
      , fullAnswer = Answer "(1 + (2 + 3)) * 4" GraphInt "(==)" []
      , conclusion = "There is an other useful law"
      }
    ,
    SubModule
      {abstract = T.intercalate "\n"
        ["Overlay is also _idempotent_: \n"
        ,">>> 1 + 1 == 1\n"
        ,"Overlaying a graph with itself is the same graph"
        ]
      , instruction = "Find a simpler and equivalent graph of: \"1 + 2 + 2 + 1 * 4\""
      , clue = "Try to use the law of idempotence"
      , fullAnswer = Answer "2 + 1 * 4" Comparison "(<=)" []
      , conclusion = "Let take a look at Connect"
      }
    ,
    SubModule
      {abstract = T.intercalate "\n"
        ["Attention (*) (connect) is __not__ commutative : \n"
        ,">>> 1 * 2 /= 2 * 1\n"
        ,"But connect is associative : \n"
        ,">>> 1 * (2 * 3) == (1 * 2) * 3\n"
        ,"If you are connecting three components, the order on which you are drawing the edges is _not_ important"
        ]
      , instruction = "Find an equivalent graph of: \"((1 * 2) * 3) + 4\""
      , clue = "Try to use the law of associativity"
      , fullAnswer = Answer "(1 * (2 * 3)) + 4" GraphInt "(==)" []
      , conclusion = "There is an other useful law"
      }
    ,
    SubModule
      {abstract = T.intercalate "\n"
        ["Connect _staturate_ : \n"
        ,">>> 1 * 1 * 1 == 1 * 1\n"
        ,"1 * 1 is a self-loop over 1"
        ,"Because you cannot have two times the same edge, the graphs are equals"
        ]
      , instruction = "Find a simpler and equivalent graph of: \"2 * 2 * 2 * 3\""
      , clue = "Try to use the saturation law"
      , fullAnswer = Answer "2 * 2 * 3" Comparison "(<=)" []
      , conclusion = "And now if we mix Connect and Overlay ?"
      }
    ,
    SubModule
      {abstract = T.intercalate "\n"
        ["(+) and (*) operators call sometimes to _distributivity_ and this is the case with algebraic graph: \n"
        ,">>> 1 * (2 + 3) == 1 * 2 + 2 * 3 \n"
        ]
      , instruction = "Find a simpler and equivalent graph of: \"1 * 2 + 1 * 3 + 1 * 4 + 3 * 4 \""
      , clue = "Try to use the distributivity law"
      , fullAnswer = Answer "1 * (2 + (3 * 4)) " Comparison "(<=)" []
      , conclusion = "Math can be fun with Graphs"
      }
  ]
  }
