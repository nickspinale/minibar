module Test where

import Control.Variable
import Minibar.Actors
import Data.Function
import Data.List

vvar :: VVar String
vvar = f <$> command 500 "date" <*> command 300 "date"

f :: Late (Maybe String) -> Late (Maybe String) -> String
f x y = defMess init x ++ " - " ++ defMess init y

maybes :: a -> a -> (b -> a) -> Late (Maybe b) -> a
maybes a0 a1 f = maybe a0 (maybe a1 f)
-- maybes = (.) `on` maybe

defMess :: (a -> String) -> Late (Maybe a) -> String
defMess = maybes "NIL" "ERR"

main :: IO ()
main = actOn vvar putStrLn
