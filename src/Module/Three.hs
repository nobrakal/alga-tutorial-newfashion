{-# LANGUAGE OverloadedStrings #-}

module Module.Three
  (mod3)
where

import Types
import qualified Data.Text as T

mod3 :: Module
mod3 = Module
  {
  name = "Useful functions and instances",
  desc = "Ok, we have the basis. Now, how can we reason over graphs ?",
  subs =
    [
    SubModule
      { abstract = "Graph is a _Functor_ instance. This means that you can use 'fmap' on graphs. It will apply a function on each vertex."
      , instruction = "You want add 1 to each vertex of a graph \"gr\". How will you do that using fmap ?"
      , clue = "Considerate \"gr\" as a list and try to add 1 to each elements"
      , fullAnswer = Answer "fmap (+ 1) gr" GraphInt "(==)" [("gr = 1 + 2 * 3 + 5 * 6","Graph Int")]
      , conclusion = "Can we do more ?"
      }
    ,
    SubModule
      { abstract = "This _Functor_ instance can do a lot."
      , instruction = "Imagine now you are wanting to replace 0 by 1 everywhere in your graph \"gr\""
      , clue = "Considerate \"gr\" as a list"
      , fullAnswer = Answer "fmap (\\x -> if x == 0 then 1 else x) gr" GraphInt "(==)" [("gr = 1 + 2 * 3 + 5 * 6","Graph Int")]
      , conclusion = "Great you have found the implementation of _replaceVertex_"
      }
    ,
    SubModule
      { abstract = "Graph is also an instance of _Foldable_"
      , instruction = "How can you tell if a vertex is in the graph or not ? Write the function name only"
      , clue = "List are also an instance of _Foldable_ the name should be the same..."
      , fullAnswer = Answer "elem" Str "(==)" []
      , conclusion = "Great you have found the implementation of _hasVertex_"
      }
    ,
    SubModule
      { abstract = "Graph is also an instance of _Traversable_"
      , instruction = "You have a graph \"gr :: Graph (Maybe Int)\", and you want to \"strip out\" the \"Maybe\", obtaining \"gr :: Maybe (Graph a)\" set to \"Nothing\" if there was a \"Nothing\" vertices or \"Just gr\" if there was only \"Just\" vertices. Can you do that ? "
      , clue = "Striping-out things is often called \"sequence\""
      , fullAnswer = Answer "sequence gr" CanFind "(==)" [("gr = Connect (pure (Just 1)) (pure (Just 2))","Graph (Maybe Int)")]
      , conclusion = "This can be also used with the IO monad to get pretty cool results"
      }
    ,
    SubModule
      { abstract = T.intercalate "\n"
        ["The fold from _Foldable_ is cool, but alga provide a cooler functions called _foldg_ :"
        , "Here is the impementation:\n"
        , "  foldg :: a -> (v -> a) -> (a -> a -> a) -> (a -> a -> a) -> Graph v -> a"
        , "  foldg e v o c gr = go gr"
        , "    where"
        , "      go Empty         = e"
        , "      go (Vertex  x  ) = v x"
        , "      go (Overlay x y) = o (go x) (go y)"
        , "      go (Connect x y) = c (go x) (go y)"
        ]
      , instruction = "If you should write a function that calculate all the \"Vertex\" of a graph \"gr\", including duplicates, how will you do using foldg ?"
      , clue = "As you can see on the definition of \"foldg\", you should provide a function for each cases possible in a graph."
      , fullAnswer = Answer "foldg 0 (const 1) (+) (+) gr" GraphInt "(==)" [("gr = 1 + 2 * 3 + 5 * 6","Graph Int")]
      , conclusion = "This is almost the implementation of _size_"
      }
    ,
    SubModule
      { abstract = "foldg can also produce a Graph"
      , instruction = "You are wanting to _transpose_ a graph \"gr\", inverting all its edges"
      , clue = "Only Connect can produce an edge, so you should try to make something _only_ with it"
      , fullAnswer = Answer "foldg Empty Vertex Overlay (flip Connect) gr" GraphInt "(==)" [("gr = 1 + 2 * 3 + 5 * 6","Graph Int")]
      , conclusion = "This is the current implementation of _transpose_"
      }
    ,
    SubModule
      { abstract = T.intercalate "\n"
        [ "Haskell alarms ! If there is a _Functor_ instance, then is there a _Monad_ one ?"
        , "Yes, this is the case for algebraic graphs, here is the definition:\n"
        , "  instance Monad Graph where"
        , "    return  = Vertex"
        , "    g >>= f = foldg Empty f Overlay Connect g\n"
        , "What does it mean ? You can turn anything into a Graph using \"Vertex\"."
        , "And if you have a function providing a graph from an argument, and a graph of the type of this argument, then folding into this graph and applying this function to each vertices also produce a _Graph_"
        ]
      , instruction = "You are wanting to add a level to a graph \"gr\", that is replacing all his vertices by an overlay of two vertices _of_ _the_ _same_ _label_ than the previous vertex. How can you do that using the bind (>>=) operator ?"
      , clue = "Write first the function that produce a graph with two vertices of the same label given as input"
      , fullAnswer = Answer "gr >>= (\\x -> x + x)" GraphInt "(==)" [("gr = 1 + 2 * 3 + 5 * 6","Graph Int")]
      , conclusion = "Powerful monads..."
      }
    ]
  }

