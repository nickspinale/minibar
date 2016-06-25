module Minibar
    ( minibar
    ) where

import System.IO
import Control.Variable
import Paths_minibar
import System.Process

minibarWith :: Handle -> Handle -> VVar (Int -> String) -> IO ()
minibarWith hin hout v = actOn v $ \f -> do
    hPutStrLn hout ""
    rows <- hGetLine hin
    hPutStrLn hout . f $ read rows

minibar :: VVar (Int -> String) -> IO ()
minibar v = do
    (w, w') <- createPipe
    (r, r') <- createPipe
    worker <- getDataFileName "data/worker"
    runProcess worker [] Nothing Nothing (Just w) (Just r') Nothing
    minibarWith r' r v
