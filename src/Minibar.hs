module Minibar
    ( minibar
    ) where

import System.IO
import Control.Variable

minibar :: Handle -> Handle -> VVar a -> (a -> Int -> String) -> IO ()
minibar hin hout v f = actOn v $ \a -> do
    hPutStrLn hout ""
    rows <- hGetLine hin
    hPutStrLn hout $ f a (read rows)
