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
      , fullAnswer = Answer "foldr (\\x y -> Vertex x + y) Empty clients" GraphInt "(==)" [("clients = [1,2,3,4,5,6,7,8,9]", "[Int]")]
      , conclusion = "The function you used is called _vertices_ in alga"
      }
    ,
    SubModule
      { abstract = "Now you are wanting to remove those who registered but are not here. You have a function \"isPresent :: Int -> Bool\" and the graph \"gr\" from the previous question. How can you obtain a \"Graph\" without the leaves for which _isPresent_ returned _False_"
      , instruction = "Maybe there is an already-made function"
      , clue = "Use _induce_ "
      , fullAnswer = Answer "induce isPresent gr" GraphInt "(==)" [("gr = vertices [1,2,3,4,5,6,7,8,9]","Graph Int"),("isPresent x = (x==1)","Int -> Bool")]
      , conclusion = "Let's continue"
      }
    ,
    SubModule
      { abstract = "The previous question was simple, now you just checked, and _isPresent_ is of type _Int_ _->_ _IO_ _Bool_ . How can you adapt you code ?"
      , instruction = "You certainly need to use the _Applicative_ instance of _Graph_ on _overlay_ and _connect_"
      , clue = "Use a combination of _sequence_ and _foldg_"
      , fullAnswer = Answer "foldg (return empty) (\\x -> (\\y -> if y then vertex x else empty) <$> isPresent x) (liftA2 overlay) (liftA2 connect) gr" IOGraphInt "(==)" [("gr = vertices [1,2,3,4,5,6,7,8,9]","Graph Int"),("isPresent x = return (x==1)","Int -> IO Bool")]
      , conclusion = "This one was tricky"
      }
    ]
  }

