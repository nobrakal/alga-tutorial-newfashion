{-# LANGUAGE OverloadedStrings #-}

module Module.Four
  (mod4)
where

import Types
import qualified Data.Text as T

mod4 :: Module
mod4 = Module
  {
  name = "An example: a network",
  desc = T.intercalate "\n"
    [ "Networks are often modelized through graphs. Let's see how we can handle that case with Alga"
    , "You are going to run a server to link people, previously registered on your super website"
    ],
  subs =
    [
    SubModule
      { abstract = "We start with a list of possible clients \"clients\". You want to turn it into an algebraic graph composed only by single vertices. Each client is represented by an id of type \"Int\""
      , instruction = "How can you do that ?"
      , clue = "It seems you are going to _Fold_ a list"
      , fullAnswer = Answer "foldr (\\x y -> Vertex x + y) Empty clients" GraphInt "(==)" [("clients = [1,2,3,4,5,6,7,8,9]", ("[Int]",True))]
      , conclusion = "The function you used is called _vertices_ in alga"
      }
    ,
    SubModule
      { abstract = "Now you are wanting to remove those who registered but are not here. You have a function \"isPresent :: Int -> Bool\" and the graph \"gr\" from the previous question. How can you obtain a \"Graph\" without the leaves for which _isPresent_ returned _False_"
      , instruction = "Maybe there is an already-made function"
      , clue = "Use _induce_ "
      , fullAnswer = Answer "induce isPresent gr" GraphInt "(==)" [("gr = vertices [1,2,3,4,5,6,7,8,9]",("Graph Int",True)),("isPresent x = (x==1)",("Int -> Bool",False))]
      , conclusion = "Let's continue"
      }
    ,
    SubModule
      { abstract = "Ok, then we will write our main loop. The first thing to do is to add a connection when requested. Imagine the id 5 has requested to get in touch with id 15"
      , instruction = "Add an edge between vertex 5 and vertex 15 (and because we work with _Directed_ _Graph_ , add an edge between vertex 15 and vertex 5"
      , clue = "There not yet wonderfull method to do that"
      , fullAnswer = Answer "(15 * 5) + (5 * 15) + gr" GraphInt "(==)" [("gr = vertices [1,2,3,4,5,6,7,8,9]",("Graph Int",True))]
      , conclusion = "Let's continue"
      }
    ]
  }

