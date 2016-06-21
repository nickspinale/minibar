module Minibar.Actors
    (
    ) where

import Control.Variable
import System.IO

command :: Int -> a -> String -> (String -> Maybe a) -> VVar (Maybe a)
command start delay cmd f = undefined

handle :: a -> Handle -> (String -> Maybe a) -> VVar (Maybe a)
handle start hndl f = undefined
