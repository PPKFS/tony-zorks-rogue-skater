module Main where

import TZRS.Prelude
import TZRS.Object
import qualified Data.Map.Strict as M
import TZRS.Store
import TZRS.Entity
import qualified Data.EnumMap as EM
import qualified Data.Vector as V
import qualified Data.IntSet as IS
import System.Random
import TZRS.Rulebook
import TZRS.RuleEffects
import TZRS.Run
import Breadcrumbs
import TZRS.World
import Rogue.Geometry.V2
import Rogue.Config
import Rogue.Window
import Rogue.Events
import Rogue.Colour
import Rogue.Geometry.Rectangle
import Rogue.Array2D.Boxed
import Effectful.State.Dynamic

screenSize :: V2
screenSize = V2 100 60

main :: IO ()
main = runEff $ runBreadcrumbs Nothing $
  evalStateShared defaultMetadata $ do
    w <- withV2 screenSize roomMap
    evalStateShared (World emptyStore w) $
      runTileMapAsState $ do
        ((%=) @_ @World) (#objects % coerced) $ EM.insert playerId (Object
          { name = "player"
          , description = ""
          , objectId = playerId
          , objectType = ObjectKind "player"
          , creationTime = Timestamp 0
          , modifiedTime = Timestamp 0
          , position = V2 20 15
          , renderable = Renderable '@' (fromRGB 0x75 0xa2 0xeb ) (Colour 0x00000000)
          , objectData = ObjectSpecifics
          })
        withWindow
          defaultWindowOptions { size = Just screenSize }
          (do
            terminalSetText "log: file='awa.log', level=trace;"
            --terminalSetText "font: 'Boxy.ttf', size=12"
          )
          (const runLoop)
          pass

defaultMetadata :: Metadata
defaultMetadata = Metadata False

data Direction = LeftDir | RightDir | UpDir | DownDir

playerId :: Entity
playerId = Entity 0

floorTile :: TileInfo
floorTile = TileInfo "floor" (Renderable '.' (Colour 0xFF313036) (Colour 0x00000000)) True

wall :: TileInfo
wall = TileInfo "wall" (Renderable '#' (Colour 0xFFb9caee) (Colour 0x00000000)) False

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

data MoveArguments = MoveArguments
  { object :: Object
  , direction :: Direction
  }

instance Display MoveArguments where
  displayBuilder _ = "move args"

moveRulebook :: Rulebook Unconstrained MoveArguments Bool
moveRulebook = (blankRulebook "move rulebook")
  { rules =
      [ cantMoveIntoWalls
      , moveIt
      ]
  }

cantMoveIntoWalls :: Rule Unconstrained MoveArguments Bool
cantMoveIntoWalls = makeRule "can't walk into walls rule" [] $ \ma -> do
  let newLoc = calculateNewLocation (object ma) (direction ma)
  ti <- getTile newLoc
  if walkable ti then rulePass else do
    traceShow "nope can't walk through a wall" pass
    return (Just False)

calculateNewLocation :: Object -> Direction -> V2
calculateNewLocation o dir = (o ^. #position) &
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
    )

moveIt :: Rule Unconstrained MoveArguments Bool
moveIt = makeRule "move rule" [] $ \ma -> do
  moveObject (getID $ object ma) (direction ma)
  return (Just True)

randomMap :: IOE :> es => Int -> Int -> Eff es Tiles
randomMap w h = do
  let (wMax, hMax) = (w-1, h-1)
  randomWalls <- mapM (const $ randomRIO (0, (w*h)-1)) [(0::Int)..500]
  let is = IS.fromList randomWalls
  let v = V.generate (w*h)
        (\i ->
          case indexToCoord w i of
            V2 0 _ -> wall
            V2 _ 0 -> wall
            V2 x y -> if x == wMax || y == hMax || (i `IS.member` is) then wall else floorTile)
  let t = Tiles
        { tileMap = Array2D (v, V2 w h)
        }
  return t

roomMap :: IOE :> es => Int -> Int -> Eff es Tiles
roomMap w h = do
  let t = Tiles { tileMap = Array2D (V.generate (w*h) (const wall), V2 w h) }
  let numRooms = 30; minSize = 6; maxSize = 10
  allPossibleRooms <- mapM (const $ do
    dims <- let d = (minSize, maxSize) in V2 <$> randomRIO d <*> randomRIO d
    pos <- let g l = subtract 1 <$> randomRIO (2, (screenSize ^. l) - (dims ^. l) - 1) in V2 <$> g _1 <*> g _2
    pure $ Rectangle pos (dims + pos)) [(0::Int)..numRooms]
  return $ snd $ foldl' (\(rooms, tm) newRoom ->
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

moveObject ::
  State World :> es
  => Entity
  -> Direction
  -> Eff es ()
moveObject e dir = do
  #objects % at e % _Just % #position %=
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
    )

runLoop ::
  State World :> es
  => State Metadata :> es
  => Breadcrumbs :> es
  => TileMap :> es
  => IOE :> es
  => Eff es ()
runLoop = do
  terminalClear
  --pl <- use #playerLocation
  renderMap
  renderObjects
  terminalRefresh
  handleEvents Blocking $ \case
    Keypress kp -> do
      putStrLn $ "Handling keypress: " <> show kp
      case asMovement kp of
        Just mvDir -> do
          pl <- use @World (#objects % at playerId)
          void $ runRulebook Nothing moveRulebook (MoveArguments (fromMaybe (error "no player") pl) mvDir)
        Nothing -> putStrLn ("unknown keypress: " <> show kp)
    WindowEvent Resize -> pass
    WindowEvent WindowClose -> ((.=) @_ @Metadata) #pendingQuit True
  ifM (use @Metadata #pendingQuit) pass runLoop

renderMap ::
  State World :> es
  => IOE :> es
  => Eff es ()
renderMap = do
  es <- use #tileMap
  traverseArrayWithCoord_ (es ^. #tileMap % coerced @_ @(Array2D TileInfo)) $ \(V2 x y) t -> do
    let r = t ^. #renderable
    terminalColour (r ^. #foreground)
    terminalBkColour (r ^. #background)
    terminalPrintText x y (one $ r ^. #glyph)

renderObjects ::
  State World :> es
  => IOE :> es
  => Eff es ()
renderObjects = do
  es <- use #objects
  forM_ es $ \v -> do
    let r = v ^. #renderable
    terminalColour (r ^. #foreground)
    terminalBkColour (r ^. #background)
    terminalPrintText (v ^. #position % _1) (v ^. #position % _2) (one $ r ^. #glyph)
