module Data.LOT
    ( LOT
    ) where

import Data.Text (Text)
import qualified Data.Text as T

data LOT = Raw Text
         | LFix Int LOT
         | RFix Int LOT
