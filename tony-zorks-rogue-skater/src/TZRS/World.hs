module TZRS.World where

import TZRS.Store
import TZRS.Object
import TZRS.Prelude
import qualified Data.Vector.Unboxed as V
import Effectful.Dispatch.Dynamic
import Effectful.TH
import qualified Data.IntMap as IM
import TZRS.Geometry

data World = World
  { objects :: Store Object
  , tileMap :: Tiles
  } deriving stock (Generic)

data TileInfo = TileInfo
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show)

data Tiles = Tiles
  { tileKinds :: IntMap TileInfo
  , tileMap :: V.Vector Int
  , dimensions :: V2
  } deriving stock (Generic, Show)

makeFieldLabelsNoPrefix ''World

data TileMap :: Effect where
  SetTile :: Int -> V2 -> TileMap m ()
  GetTile :: V2 -> TileMap m TileInfo
  GetTileKindId :: V2 -> TileMap m Int

makeEffect ''TileMap

runTileMapAsState ::
  State World :> es
  => Eff (TileMap : es) a
  -> Eff es a
runTileMapAsState = interpret $ \_env -> \case
  SetTile tileKind pos -> error ""
  GetTile pos -> do
    tm <- use #tileMap
    return $ getTileInfo tm pos
  GetTileKindId pos -> do
    tm <- use #tileMap
    return $ getTileAt tm pos

indexToCoord :: Int -> Int -> V2
indexToCoord w i = V2 (i `mod` w) (i `div` w)

coordToIndex :: Int -> V2 -> Int
coordToIndex w (V2 x y) = y*w + x

getTileAt :: Tiles -> V2 -> Int
getTileAt tm pos = view #tileMap tm V.! coordToIndex (view (#dimensions % _1) tm) (coerce pos)

getTileInfo :: Tiles -> V2 -> TileInfo
getTileInfo tm pos =
  let ti = getTileAt tm pos
  in fromMaybe (error "") $ ti `IM.lookup` (tm ^. #tileKinds)