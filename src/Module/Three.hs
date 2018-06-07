{-# LANGUAGE OverloadedStrings #-}

module Module.Three
  (mod3)
where

import Types
import qualified Data.Text as T

mod3 :: Module
mod3 = Module
  {
  name = "Useful functions",
  desc = "Ok, we have the basis. Now, how can we reason over graphs ?",
  subs =
    [
    SubModule
      { abstract = "Graph are a _Functor_ instance. This means that you can use fmap on graphs. It will apply a function on each vertex"
      , instruction = "You want add 1 to each vertex of a graph \"gr\". How will you do that using fmap ?"
      , clue = "Considerate \"gr\" as a list and try to add 1 to each elements"
      , fullAnswer = Answer "fmap (+ 1) gr" GraphInt "(==)" ["gr = 1 + 2 * 3 + 5 * 6"]
      , conclusion = "Can we do more ?"
      }
    ,
    SubModule
      { abstract = "This _Functor_ instance can do a lot."
      , instruction = "Imagine now you are wanting to replace 0 by 1 everywhere in your graph \"gr\""
      , clue = "Considerate \"gr\" as a list"
      , fullAnswer = Answer "fmap (\\x -> if x == 0 then 1 else x) gr" GraphInt "(==)" ["gr = 1 + 2 * 3 + 5 * 6"]
      , conclusion = "Great you have found the implementation of _replaceVertex_"
      }
    ,
    SubModule
      { abstract = T.intercalate "\n"
        ["Graph are foldable, and provide a cool functions called _foldg_ :"
        , "Here is the impementation:\n"
        , "foldg :: a -> (v -> a) -> (a -> a -> a) -> (a -> a -> a) -> Graph v -> a"
        , "foldg e v o c gr = go gr"
        , "where"
        , "  go Empty         = e"
        , "  go (Vertex  x  ) = v x"
        , "  go (Overlay x y) = o (go x) (go y)"
        , "  go (Connect x y) = c (go x) (go y)"
        ]
      , instruction = "If you should write a function that calculate all the \"Vertex\" of a graph \"gr\", including duplicates, how will you do using foldg ?"
      , clue = "As you can see on the definition of \"foldg\", you should provide a function for each cases possible in a graph."
      , fullAnswer = Answer "foldg 0 (const 1) (+) (+) gr" GraphInt "(==)" ["gr = 1 + 2 * 3 + 5 * 6"]
      , conclusion = "This is almost the implementation of _size_"
      }
    ,
    SubModule
      { abstract = "foldg can also produce a Graph"
      , instruction = "You are wanting to _transpose_ a graph \"gr\", inverting all its edges"
      , clue = "Only Connect can produce an edge, so you should try to make something _only_ with it"
      , fullAnswer = Answer "foldg Empty Vertex Overlay (flip Connect) gr" GraphInt "(==)" ["gr = 1 + 2 * 3 + 5 * 6"]
      , conclusion = "This is the current implementation of _transpose_"
      }
    ]
  }

