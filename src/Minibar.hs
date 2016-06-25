module Minibar
    ( minibar
    ) where

import System.IO
import Control.Variable
import System.Process

minibarWith :: Handle -> Handle -> VVar (Int -> String) -> IO ()
minibarWith hin hout v = actOn v $ \f -> do
    hPutStrLn hin ""
    rows <- hGetLine hout
    hPutStrLn hin . f $ read rows

minibar :: VVar (Int -> String) -> IO ()
minibar v = do
    (inR, inW) <- createPipe
    (outR, outW) <- createPipe
    hSetBuffering inW NoBuffering
    hSetBuffering outW NoBuffering
    runProcess "/bin/sh" ["-c", script] Nothing Nothing (Just inR) (Just outW) Nothing
    minibarWith inW outR v

script :: String
script = unlines
    [ "id=$RANDOM"
    , "prefix='minibar'"
    , ""
    , "fifo1=\"${TMPDIR:-/tmp}/$prefix-fifo1-$id\""
    , "fifo2=\"${TMPDIR:-/tmp}/$prefix-fifo2-$id\""
    , ""
    , "trap \"rm -rf $fifo1 $fifo2\" EXIT SIGINT SIGTERM"
    , ""
    , "mkfifo -m o+w $fifo1"
    , "mkfifo -m o+w $fifo2"
    , ""
    , "cmd=\""
    , "  echo -en '\\033[8;1;0t' > /dev/tty"
    , "  (while read -rs; do"
    , "      tput cols"
    , "      read -rs y"
    , "      tput reset > /dev/tty"
    , "      echo -n \\$y > /dev/tty"
    , "      setterm -cursor off > /dev/tty"
    , "  done) < $fifo1 > $fifo2"
    , "\""
    , ""
    , "xterm -title statusline -geometry 80x1+0+0 -e sh -c \"$cmd\" &> /dev/null &"
    , ""
    , "cat $fifo2 &"
    , "cat <&0 > $fifo1"
    ]
