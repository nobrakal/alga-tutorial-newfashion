{-# LANGUAGE OverloadedStrings #-}

module Module.Zero
  (mod0)
where

import Types
import qualified Data.Text as T

mod0 :: Module
mod0 = Module
  {
  name = "Basics",
  desc = "Algebraic graphs ? Again mathematics and computer science mixed ? Follow the tour.",
  subs = [s00, s01, s02, s03, s04]
  }

s00 :: SubModule
s00  = SubModule
  { abstract = "Imagine you are a skillful haskell developper, and you want to define a graph structure recursively. What is the base case ? An empty graph."
  , instruction = "How would you describe an empty graph ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Empty" "Graph Int" "(==)"
  , conclusion = "You are starting to understand"
  }

s01 :: SubModule
s01  = SubModule
  { abstract = "Ok then come an other base case: a single vertex"
  , instruction = "How would you describe a single vertex identified by \"0\" ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Vertex 0" "Graph Int" "(==)"
  , conclusion = "Lets see after"
  }

s02 :: SubModule
s02  = SubModule
  { abstract = "Next, you are wanting to put several vertices together, lets say by overlaying them"
  , instruction = "How would you describe a graph that Overlay a single vertex 0 and a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Overlay (Vertex 0) (Vertex 1)" "Graph Int" "(==)"
  , conclusion = "So Overlay is commutative: Overlay (Vertex 0) (Vertex 1) == Overlay (Vertex 1) (Vertex 0)... Is there other maths here ?"
  }

s03 :: SubModule
s03  = SubModule
  { abstract = "Now you are wanting to connect them by an edge. Algebraic graph represents directed graphs, so an edge from 0 to 1 is not the same than an edge from 1 to 0"
  , instruction = "How would you describe a graph that Connect a single vertex 0 to a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Connect (Vertex 0) (Vertex 1)" "Graph Int" "(==)"
  , conclusion = "Well you have found the definition of the Graph in Alga: \n\ndata Graph a = Empty\n" `T.append` T.unlines (map (T.append (T.replicate 13 (T.singleton ' ')))
        [ "| Vertex a"
        , "| Overlay (Graph a) (Graph a)"
        , "| Connect (Graph a) (Graph a)"
        ])
  }

s04 :: SubModule
s04 = SubModule
  { abstract = "Is that all ? Yes, almsot. As you noticed, you can Overlay and Connect any graph. Connecting two graphs correspond to add an edge between each vertex from the right side to the left one"
  , instruction = "For example, try to find a condensed representation the graph composed by the following edges: (0,1), (0,2), (1,2)"
  , clue = "Use Connect wisely, you don't need to overlay anything"
  , fullAnswer = Answer "Connect (Vertex 0) (Connect (Vertex 1) (Vertex 2))" "Graph Int" "(===)"
  , conclusion = "You know the alga's basics :)"
  }

