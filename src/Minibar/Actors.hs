module Minibar.Actors
    ( command
    , handle
    ) where

import Control.Variable
import System.IO
import System.Exit
import System.Process
import Control.Concurrent
import Control.Monad

command :: Int -> String -> VVar (Maybe (Maybe String))
command delay cmd = actor $ \sink -> forever $ do
    (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
    case code of
        ExitSuccess -> sink $ Just out
        ExitFailure _ -> sink Nothing
    threadDelay $ 10000 * delay

handle :: Handle -> VVar (Maybe (Maybe String))
handle hndl = undefined
