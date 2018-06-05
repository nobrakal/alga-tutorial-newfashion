{-# LANGUAGE OverloadedStrings #-}

module Module.Zero
  (mod0)
where

import Types
import qualified Data.Text as T

mod0 :: Module
mod0 = Module
  {
  desc = "Algebraic graphs ? Again mathematics and computer science mixed ? Follow the tour.",
  subs = [s00, s01, s02, s03, s03]
  }

s00 :: SubModule
s00  = SubModule
  { abstract = "Imagine you are a skillful haskell developper, and you want to define a graph structure recursively. What is the base case ? An empty graph."
  , instruction = "How would you describe an empty graph ?"
  , clue = "Maybe simply by writing the thing down"
  , answer = Left "Empty"
  , conclusion = "You are starting to understand"
  }

s01 :: SubModule
s01  = SubModule
  { abstract = "Ok then come an other base case: a single vertex"
  , instruction = "How would you describe a single vertex identified by \"0\" ?"
  , clue = "Maybe simply by writing the thing down"
  , answer = Left "Vertex 0"
  , conclusion = "Lets see after"
  }

s02 :: SubModule
s02  = SubModule
  { abstract = "Next, you are wanting to put several vertices together, lets say by overlaying them"
  , instruction = "How would you describe a graph that Overlay a single vertex 0 and a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , answer = Left "Overlay (Vertex 0) (Vertex 1)"
  , conclusion = "So Overlay is commutative: Overlay (Vertex 0) (Vertex 1) == Overlay (Vertex 1) (Vertex 0)... Is there other maths here ?"
  }

s03 :: SubModule
s03  = SubModule
  { abstract = "Oh, and last but not least, you are wanting to connect them by an edge. Algebraic graph represents directed graphs, so an edge from 0 to 1 is not the same than an edge from 1 to 0"
  , instruction = "How would you describe a graph that Connect a single vertex 0 to a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , answer = Left "Connect (Vertex 0) (Vertex 1)"
  , conclusion = "Well you have found the definition of the Graph in Alga: \ndata Graph a = Empty\n" `T.append` T.unlines (map (T.append (T.replicate 13 (T.singleton ' ')))
        [ "| Vertex a"
        , "| Overlay (Graph a) (Graph a)"
        , "| Connect (Graph a) (Graph a)"
        ])
  }
