module Data.LOT
    ( LOT
    ) where

import Data.Text (Text)
import qualified Data.Text as T

data LOT = LOT Side Piece
data Piece = Raw String | Stack Int [LOT]

data Side = L | R

-- data LOT = Raw String
--          | Left LOT LOT
--          | Right LOT LOT
--          | Center LOT LOT LOT

-- renderWidth :: Int -> LOT -> Maybe String
-- renderWidth w (Raw str) | length str > w = Nothing

