module TZRS.World where

import TZRS.Store
import TZRS.Object
import TZRS.Prelude
import Effectful.Dispatch.Dynamic
import Effectful.TH
import qualified Rogue.Tilemap as TM
import Rogue.Array2D.Boxed
import Rogue.FieldOfView.Visibility
import qualified Data.Vector as V
import Rogue.Geometry.Rectangle
import Rogue.Colour
import System.Random
import qualified Data.IntSet as IS
import Rogue.Geometry.Line
import TZRS.Entity
import Rogue.Rendering.Viewport

data World = World
  { objects :: Store Object
  , tileMap :: Tiles
  , dirtyViewsheds :: [Entity]
  , turn :: Timestamp
  , entityCounter :: Entity
  , viewports :: Viewports
  , player :: Entity
  , randomTest :: Integer
  } deriving stock (Generic)

data Viewports = Viewports
  { mapViewport :: Viewport ()
  , bottomViewport :: Viewport ()
  , sideViewport :: Viewport ()
  } deriving stock (Generic)

instance AsLayer () where
  toLayer () = 0

data TileInfo = TileInfo
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show)

data Tiles = Tiles
  { tileMap :: Array2D TileInfo
  , revealedTiles :: Array2D Bool
  , rooms :: [Rectangle]
  } deriving stock (Generic, Show)

makeFieldLabelsNoPrefix ''Tiles

instance TM.Tilemap Tiles TileInfo where
  getTile = TM.getTile . view #tileMap
  setTile tm t p = tm & #tileMap %~ (\tm' -> TM.setTile tm' t p)
  setTiles tm ls = tm & #tileMap %~ (`TM.setTiles` ls)

instance VisibilityMap Tiles where
  positionBlocksVisibility t p = not $ view #walkable $ TM.getTile t p
  dimensions (Tiles (Array2D (_, d)) _ _) = d

makeFieldLabelsNoPrefix ''World

tickTurn :: State World :> es => Eff es ()
tickTurn = #turn %= (+1)

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


floorTile :: TileInfo
floorTile = TileInfo "floor" (Renderable '.' (Colour 0xFF008888) (Colour 0x00000000)) True

wall :: TileInfo
wall = TileInfo "wall" (Renderable '#' (Colour 0xFF00FF00) (Colour 0x00000000)) False

digRectangle ::
  Rectangle
  -> Tiles
  -> Tiles
digRectangle r = #tileMap %~ (\x -> x //@ map (,floorTile) (rectanglePoints Horizontal r))

digHorizontalTunnel ::
  V2
  -> Int
  -> Tiles
  -> Tiles
digHorizontalTunnel p l = #tileMap %~ (\x -> x //@ map (\i -> (p & _1 %~ (+ if l > 0 then i else -i),floorTile)) [0..(abs l)] )

digVerticalTunnel ::
  V2
  -> Int
  -> Tiles
  -> Tiles
digVerticalTunnel p l = #tileMap %~ (\x -> x //@ map (\i -> (p & _2 %~ (+ if l > 0 then i else -i), floorTile)) [0..(abs l)] )

randomWalls :: IOE :> es => V2 -> Int -> Eff es Tiles
randomWalls (V2 w h) numWalls = do
  let (wMax, hMax) = (w-1, h-1)
  walls <- mapM (const $ randomRIO (0, (w*h)-1)) [(0::Int)..numWalls]
  let is = IS.fromList walls
  let v = V.generate (w*h)
        (\i ->
          case indexToCoord w i of
            V2 0 _ -> wall
            V2 _ 0 -> wall
            V2 x y -> if x == wMax || y == hMax || (i `IS.member` is) then wall else floorTile)
  return Tiles { tileMap = Array2D (v, V2 w h), revealedTiles = Array2D (V.generate (w*h) (const False), V2 w h), rooms = [] }

roomMap :: IOE :> es => V2 -> Int -> Int -> Eff es Tiles
roomMap screenSize w h = do
  let t = Tiles { tileMap = Array2D (V.generate (w*h) (const wall), V2 w h), revealedTiles = Array2D (V.generate (w*h) (const False), V2 w h), rooms = [] }
  let numRooms = 30; minSize = 6; maxSize = 10
  allPossibleRooms <- mapM (const $ do
    dims <- let d = (minSize, maxSize) in V2 <$> randomRIO d <*> randomRIO d
    pos <- let g l = subtract 1 <$> randomRIO (2, (screenSize ^. l) - (dims ^. l) - 1) in V2 <$> g _1 <*> g _2
    pure $ Rectangle pos (dims + pos)) [(0::Int)..numRooms]
  let (rs, tiles) = foldl' (\(rooms, tm) newRoom ->
        if any (rectanglesIntersect newRoom) rooms then {- ignore -} (rooms, tm)
        else
          let digStuff lastRoom =
                let newP@(V2 newX newY) = centre newRoom
                    oldP@(V2 prevX prevY) = centre lastRoom
                in
                  if even (length rooms)
                  then digVerticalTunnel oldP (newY - prevY) . digHorizontalTunnel newP (prevX - newX)
                  else digHorizontalTunnel oldP (newX - prevX) . digVerticalTunnel newP (prevY - newY)
              updF = maybe id digStuff $ listToMaybe rooms
          in (newRoom:rooms, updF $ digRectangle newRoom tm)) ([], t) allPossibleRooms
  return (tiles & #rooms .~ rs)

roomMap2 :: Int -> Int -> Tiles
roomMap2 w h = do
  let t = Tiles { tileMap = Array2D (V.generate (w*h) (const wall), V2 w h), revealedTiles = Array2D (V.generate (w*h) (const False), V2 w h), rooms = [] }
    in foldl' (\tm v -> digRectangle (Rectangle v v) tm) t (mconcat $ circleRays (V2 30 30) 15)