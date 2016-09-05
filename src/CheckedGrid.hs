module CheckedGrid
( CheckedGrid
, elem
, Unit (..)
, u0
, toList
, createCheckedGrid
) where

import           Grid (Grid, Unit(..), u0)
import qualified Grid as Grid

import Prelude hiding (elem)

data CheckedGrid = CheckedGrid Unit Unit Grid
                   deriving (Eq, Show)

createCheckedGrid :: Unit -> Unit -> CheckedGrid
createCheckedGrid height width = CheckedGrid height width (initGrid height width)

initGrid :: Unit -> Unit -> Grid
initGrid (Unit height) (Unit width) 
  = Grid.fromList [(x, [u0..(Unit $ height - 1)]) | x <- [u0..(Unit $ width - 1)] ]

__grid :: CheckedGrid -> Grid
__grid (CheckedGrid _ _ g) = g

elem :: (Unit, Unit) -> CheckedGrid -> Bool
elem u = (Grid.elem u . __grid)

toList :: CheckedGrid -> [(Unit, Unit)]
toList = Grid.toList . __grid