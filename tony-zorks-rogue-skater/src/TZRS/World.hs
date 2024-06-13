module TZRS.World where

import TZRS.Store
import TZRS.Object
import TZRS.Prelude
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Rogue.Geometry.V2
import qualified Rogue.Tilemap as TM
import Rogue.Array2D.Boxed
import Effectful.State.Dynamic (State)

data World = World
  { objects :: Store Object
  , tileMap :: Tiles
  } deriving stock (Generic)

data TileInfo = TileInfo
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show)

newtype Tiles = Tiles
  { tileMap :: Array2D TileInfo
  } deriving stock (Generic, Show)

instance TM.Tilemap Tiles TileInfo where
  getTile = TM.getTile . coerce @_ @(Array2D TileInfo)
  setTile (Tiles a) t = Tiles . TM.setTile a t
  setTiles (Tiles a) = Tiles . TM.setTiles a

makeFieldLabelsNoPrefix ''World

data TileMap :: Effect where
  SetTile :: TileInfo -> V2 -> TileMap m ()
  SetTiles :: [(V2, TileInfo)] -> TileMap m ()
  GetTile :: V2 -> TileMap m TileInfo

makeEffect ''TileMap

runTileMapAsState ::
  State World :> es
  => Eff (TileMap : es) a
  -> Eff es a
runTileMapAsState = interpret $ \_env -> \case
  SetTile tileKind pos -> #tileMap %= (\tm -> TM.setTile tm tileKind pos)
  GetTile pos -> (`TM.getTile` pos) <$> use #tileMap
  SetTiles ls -> #tileMap %= (`TM.setTiles` ls)