module Types where

import qualified Data.Text as T

type Named a = (String,a)

data Module = Module {
  desc :: T.Text,
  subs :: [SubModule]
                     }

data SubModule = SubModule {
  abstract :: T.Text,
  instructions :: T.Text,
  clue :: T.Text,
  answer :: Either T.Text T.Text -- Either a function or an expression
}
