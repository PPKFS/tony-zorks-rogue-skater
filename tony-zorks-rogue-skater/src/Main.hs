module Main where

import TZRS.Prelude
import TZRS.Object
import BearLibTerminal.Raw
    ( terminalBkColorUInt,
      terminalClear,
      terminalColorUInt,
      terminalPrintText,
      terminalRefresh,
      terminalSetText )
import BearLibTerminal
import qualified Data.Map.Strict as M
import TZRS.Store
import TZRS.Entity
import qualified Data.EnumMap as EM
import Foreign.C.Types (CUInt(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import System.Random
import TZRS.Rulebook
import TZRS.RuleEffects
import TZRS.Run
import Breadcrumbs
import TZRS.World
import TZRS.Geometry

screenSize :: (Int, Int)
screenSize = (100, 60)

data Rectangle = Rectangle
  { topLeft :: V2
  , bottomRight :: V2
  } deriving stock (Show, Generic)

rectanglesIntersect ::
  Rectangle
  -> Rectangle
  -> Bool
rectanglesIntersect r1 r2 = not $
    ((r1 ^. #topLeft % _1 >= r2 ^. #bottomRight % _1) || (r2 ^. #topLeft % _1 >= r1 ^. #bottomRight % _1))
    ||
    ((r1 ^. #topLeft % _2 >= r2 ^. #bottomRight % _2) || (r2 ^. #topLeft % _2 >= r1 ^. #bottomRight % _2))

centre :: Rectangle -> V2
centre (Rectangle (V2 x1 y1) (V2 x2 y2)) = V2 ((x1+x2) `div` 2) ((y1+y2) `div` 2)

main :: IO ()
main = runEff $ runBreadcrumbs Nothing $
  evalStateShared defaultMetadata $ do
    w <- uncurry roomMap screenSize
    evalStateShared (World emptyStore w) $ do
      ((%=) @_ @World) (#objects % coerced) $ EM.insert playerId (Object
        { name = "player"
        , description = ""
        , objectId = playerId
        , objectType = ObjectKind "player"
        , creationTime = Timestamp 0
        , modifiedTime = Timestamp 0
        , position = V2 20 15
        , renderable = Renderable '@' (Colour 0xFFF33FFF) (Colour 0x00000000)
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

type Point = (Int, Int)

defaultMetadata :: Metadata
defaultMetadata = Metadata False

data Direction = LeftDir | RightDir | UpDir | DownDir

playerId :: Entity
playerId = Entity 0

defaultTileKinds :: IntMap TileInfo
defaultTileKinds = IM.fromList $ zip [0..]
  [ TileInfo "floor" (Renderable '.' (Colour 0xFF313036) (Colour 0x00000000)) True
  , TileInfo "wall" (Renderable '#' (Colour 0xFFb9caee) (Colour 0x00000000)) False
  ]

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  ]

type Color = Word32

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
  tm <- use @World #tileMap
  let ti = getTileInfo tm newLoc
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
  let (wallId, floorId) = (1, 0)
  let (wMax, hMax) = (w-1, h-1)
  randomWalls <- mapM (const $ randomRIO (0, (w*h)-1)) [(0::Int)..500]
  let is = IS.fromList randomWalls
  let v = V.generate (w*h)
        (\i ->
          case indexToCoord w i of
            V2 0 _ -> wallId
            V2 _ 0 -> wallId
            V2 x y -> if x == wMax || y == hMax || (i `IS.member` is) then wallId else floorId)
  let t = Tiles
        { tileKinds = defaultTileKinds
        , tileMap = v
        , dimensions = V2 w h
        }
  return t

roomMap :: IOE :> es => Int -> Int -> Eff es Tiles
roomMap w h = do
  let (wallId, floorId) = (1, 0)
  let (wMax, hMax) = (w-1, h-1)
  let v = V.generate (w*h) (const wallId)
  let t = Tiles
        { tileKinds = defaultTileKinds
        , tileMap = v
        , dimensions = V2 w h
        }
  let numRooms = 30
      minSize = 6
      maxSize = 10
  allPossibleRooms <- mapM (const $ do

    w' <- randomRIO (minSize, maxSize)
    h' <- randomRIO (minSize, maxSize)
    x <- subtract 1 <$> randomRIO (2, fst screenSize - w' - 1)
    y <- subtract 1 <$> randomRIO (2, snd screenSize - h' - 1)
    pure $ Rectangle (V2 x y) (V2 (x+w') (y+h')) ) [(0::Int)..numRooms]
  return $ snd $ foldl'
    (\(rooms, tm) newRoom ->
      if any (rectanglesIntersect newRoom) rooms
        then {- ignore -} (rooms, tm)
        else
          let updF = case rooms of
                [] -> id
                (lastRoom:_) ->
                  let newP@(V2 newX newY) = centre newRoom
                      oldP@(V2 prevX prevY) = centre lastRoom
                  in
                    if even (length rooms)
                    then
                      traceShow (mconcat ["digging horizontal first from ", show newP, show (newX - prevX), "then ", show oldP, show (newY - prevY)])
                        $ digVerticalTunnel oldP (newY - prevY) . digHorizontalTunnel newP (prevX - newX)
                    else
                      traceShow (mconcat ["digging vertical first from ", show newP, show (prevY - newY), "then ", show oldP, show (newX - prevX)])
                        $ digHorizontalTunnel oldP (newX - prevX) . digVerticalTunnel newP (prevY - newY)
          in (newRoom:rooms, updF $ digRectangle newRoom tm)) ([], t) allPossibleRooms

rectanglePoints ::
  Rectangle
  -> [V2]
rectanglePoints r = do
  x <- [(r ^. #topLeft % _1) .. (r ^. #bottomRight % _1)]
  V2 x <$> [(r ^. #topLeft % _2) .. (r ^. #bottomRight % _2)]

digRectangle ::
  Rectangle
  -> Tiles
  -> Tiles
digRectangle r t =
  let floorId = 0
      width = t ^. #dimensions % _1
  in
    t & #tileMap %~ (\x -> x V.// map (\p -> (coordToIndex width p,floorId)) (rectanglePoints r))

digHorizontalTunnel ::
  V2
  -> Int
  -> Tiles
  -> Tiles
digHorizontalTunnel p l t =
  let floorId = 0
      width = t ^. #dimensions % _1
  in t & #tileMap %~ (\x -> x V.// map (\i -> (coordToIndex width (p & _1 %~ (+ if l > 0 then i else -i)),floorId)) [0..(abs l)] )

digVerticalTunnel ::
  V2
  -> Int
  -> Tiles
  -> Tiles
digVerticalTunnel p l t =
  let floorId = 0
      width = t ^. #dimensions % _1
  in t & #tileMap %~ (\x -> x V.// map (\i -> (coordToIndex width (p & _2 %~ (+ if l > 0 then i else -i)),floorId)) [0..(abs l)] )

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
  V.iforM_ (es ^. #tileMap) $ \i v -> do
    let t = fromMaybe (error "") $ v `IM.lookup` (es ^. #tileKinds)
    let r = t ^. #renderable
    let V2 x y = indexToCoord (es ^. #dimensions % _1) i
    terminalColorUInt $ CUInt . toWord32 $ (r ^. #foreground)
    terminalBkColorUInt $ CUInt . toWord32 $ (r ^. #background)
    terminalPrintText x y (one $ r ^. #glyph)
renderObjects ::
  State World :> es
  => IOE :> es
  => Eff es ()
renderObjects = do
  es <- use #objects
  forM_ es $ \v -> do
    let r = v ^. #renderable
    terminalColorUInt $ CUInt . toWord32 $ (r ^. #foreground)
    terminalBkColorUInt $ CUInt . toWord32 $ (r ^. #background)
    terminalPrintText (v ^. #position % _1) (v ^. #position % _2) (one $ r ^. #glyph)
