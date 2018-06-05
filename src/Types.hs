module Types where

import qualified Data.Text as T

data Module = Module {
  desc :: T.Text,
  subs :: [SubModule]
                     }

data SubModule = SubModule {
  abstract :: T.Text,
  instruction :: T.Text,
  clue :: T.Text,
  answer :: Either T.Text T.Text, -- Either a function or an expression
  conclusion :: T.Text
}
