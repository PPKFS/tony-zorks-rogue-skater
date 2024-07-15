module Main where

import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic

import Rogue.Array2D.Boxed
import Rogue.Colour
import Rogue.Config
import Rogue.Events
import Rogue.FieldOfView.Visibility
import Rogue.Geometry.Rectangle
import Rogue.ObjectQuery
import Rogue.Rendering.Viewport
import Rogue.Window

import TZRS.Entity
import TZRS.Object
import TZRS.Prelude
import TZRS.RuleEffects
import TZRS.Rulebook
import TZRS.Store
import TZRS.World
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random (randomIO)
import TZRS.Viewshed
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Rogue.AStar

screenSize :: V2
screenSize = V2 130 80

mapViewportRectangle :: Rectangle
mapViewportRectangle = Rectangle (V2 0 0) (screenSize-V2 20 15)

bottomViewportRectangle :: Rectangle
bottomViewportRectangle = rectangleFromDimensions
  (V2 0 (bottomEdge mapViewportRectangle + 1))
  (V2 (view _1 screenSize - 20) 14)

sideViewportRectangle :: Rectangle
sideViewportRectangle = rectangleFromDimensions
  (V2 (view _1 (bottomRight mapViewportRectangle)) 0)
  (V2 20 (view _2 screenSize))

makeViewports :: Viewports
makeViewports = Viewports
  { mapViewport = Viewport mapViewportRectangle (Just ((), Colour 0xFF000000))
  , bottomViewport = Viewport bottomViewportRectangle (Just ((), Colour 0xFF222222))
  , sideViewport = Viewport sideViewportRectangle (Just ((), Colour 0xFF222222))
  }

main :: IO ()
main = runEff $ runBreadcrumbs Nothing $
  evalStateShared defaultMetadata $ do
    w <- withV2 (bottomRight mapViewportRectangle) (roomMap (bottomRight mapViewportRectangle))
    evalStateShared (World emptyStore w [] (Timestamp 0) (Entity 0) makeViewports (Entity (-1)) 0) $
      runQueryAsState $
        withWindow
          defaultWindowOptions { size = Just screenSize }
          (do
            terminalSetText "log: file='awa.log', level=trace;"
            terminalSetText "font: 'Boxy.ttf', codepage=437, size=18"
            buildWorld
          )
          (const runLoop)
          pass

makeAllViewshedsDirty :: (State World :> es, ObjectQuery Object :> es) => Eff es ()
makeAllViewshedsDirty = traverseObjects $ \t -> do
  let mbVs = getViewshedMaybe t
  whenJust mbVs $ const $ #dirtyViewsheds %= (getID t :)
  return Nothing

makeObject ::
  State World :> es
  => ObjectQuery Object :> es
  => Text
  -> ObjectKind
  -> V2
  -> Renderable
  -> ObjectSpecifics
  -> (Object -> Object)
  -> Eff es Entity
makeObject name kind pos renderable spec f = do
  t <- use #turn
  e <- generateEntity
  setObject $ f $ Object
    { name
    , description = ""
    , objectId = e
    , objectType = kind
    , creationTime = t
    , modifiedTime = t
    , position = pos
    , renderable
    , objectData = spec
    , occupiesTile = True
    }
  return e

playerRenderable :: Renderable
playerRenderable = Renderable '@' (fromRGB 0x75 0xa2 0xeb) (Colour 0x00000000)

playerData :: ObjectSpecifics
playerData = PlayerSpecifics $ Player { viewshed = Viewshed S.empty 20}

goblinRenderable :: Renderable
goblinRenderable = Renderable 'g' (fromRGB 255 30 30) (Colour 0x00000000)

orcRenderable :: Renderable
orcRenderable = Renderable 'o' (fromRGB 220 50 30) (Colour 0x00000000)

goblinData :: ObjectSpecifics
goblinData = MonsterSpecifics $ Monster { viewshed = Viewshed S.empty 20}

buildWorld ::
  State World :> es
  => ObjectQuery Object :> es
  => IOE :> es
  => Eff es ()
buildWorld = do
  rooms <- use @World (#tileMap % #rooms)
  let playerPos = case listToMaybe rooms of
        Nothing -> V2 20 15
        Just x -> centre x + V2 1 1
  p <- makeObject "player" (ObjectKind "player") playerPos playerRenderable playerData id
  #player .= p

  forM_ (zip [1..] rooms) $ \(i, room) -> do
    monsterChoice <- randomIO @Bool
    if monsterChoice then
      makeObject ("goblin #" <> show i) (ObjectKind "monster") (centre room) goblinRenderable goblinData id
    else
      makeObject ("orc #" <> show i) (ObjectKind "monster") (centre room) orcRenderable goblinData id
  makeAllViewshedsDirty
  updateViewsheds

defaultMetadata :: Metadata
defaultMetadata = Metadata False

data Direction = LeftDir | RightDir | UpDir | DownDir | UpLeftDir | DownRightDir | UpRightDir | DownLeftDir

playerId :: Entity
playerId = Entity 0

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
  bm <- (!@ newLoc) <$> use @World (#tileMap % #walkableTiles)
  if bm then rulePass else return (Just False)

calculateNewLocation :: Object -> Direction -> V2
calculateNewLocation o dir = (o ^. #position) &
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
      UpRightDir -> (\(V2 x y) -> V2 (x+1) (y-1))
      DownRightDir -> (\(V2 x y) -> V2 (x+1) (y+1))
      UpLeftDir -> (\(V2 x y) -> V2 (x-1) (y-1))
      DownLeftDir -> (\(V2 x y) -> V2 (x-1) (y+1))
    )

moveIt :: Rule Unconstrained MoveArguments Bool
moveIt = makeRule "move rule" [] $ \ma -> do
  moveObject (getID $ object ma) (direction ma)
  return (Just True)

runQueryAsState ::
  State World :> es
  => Eff (ObjectQuery Object : es) a
  -> Eff es a
runQueryAsState = interpret $ \env -> \case
  GenerateEntity -> #entityCounter <<%= (+1)
  SetObject r -> do
    #objects % at (getID r) %= updateIt r
    when (occupiesTile r) $ #tileMap % #walkableTiles %= \wt -> wt // (position r, False)
    --whenJust (getViewshedMaybe r) $ const (makeViewshedDirty . getID $ r)
  GetObject e -> do
    let i = getID e
    mbObj <- use $ #objects % at i
    case mbObj of
      Nothing -> error $ "Could not find object with id " <> show i
      Just x -> return x
  TraverseObjects f -> do
    m <- use #objects
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r (\r' -> localSeqUnlift env $ \unlift -> unlift $ setObject r')) m

updateIt :: a -> Maybe a -> Maybe a
updateIt newObj mbExisting = case mbExisting of
  Nothing -> Just newObj
  Just _ -> Just newObj

moveObject ::
  ObjectQuery Object :> es
  => State World :> es
  => Entity
  -> Direction
  -> Eff es ()
moveObject e dir = do
  o <- getObject e
  when (occupiesTile o) $ #tileMap % #walkableTiles %= \wt -> wt // (position o, True)
  let newPos = position o &
        (case dir of
          LeftDir -> _1 %~ subtract 1
          RightDir -> _1 %~ (+1)
          UpDir -> _2 %~ subtract 1
          DownDir -> _2 %~ (+1)
          UpRightDir -> (\(V2 x y) -> V2 (x+1) (y-1))
          DownRightDir -> (\(V2 x y) -> V2 (x+1) (y+1))
          UpLeftDir -> (\(V2 x y) -> V2 (x-1) (y-1))
          DownLeftDir -> (\(V2 x y) -> V2 (x-1) (y+1))
        )
  modifyObject e (#position .~ newPos)
  #dirtyViewsheds %= (e:)

runLoop ::
  State World :> es
  => ObjectQuery Object :> es
  => State Metadata :> es
  => Breadcrumbs :> es
  => IOE :> es
  => Eff es ()
runLoop = do
  everyTurn
  vt <- getVisibleTiles
  terminalClear
  renderMap vt
  renderObjects vt
  renderBottomTerminal
  terminalRefresh
  handleEvents Blocking $ \case
    Keypress kp -> do
      --putStrLn $ "Handling keypress: " <> show kp
      case asMovement kp of
        Just mvDir -> do
          pl <- use @World #player >>= getObject
          void $ runRulebook Nothing moveRulebook (MoveArguments pl mvDir)
        Nothing -> putStrLn ("unknown keypress: " <> show kp)
      when (kp == TkEsc) $ ((.=) @_ @Metadata) #pendingQuit True
    WindowEvent Resize -> pass
    WindowEvent WindowClose -> ((.=) @_ @Metadata) #pendingQuit True
  ifM (use @Metadata #pendingQuit) pass runLoop

getVisibleTiles ::
  ObjectQuery Object :> es
  => Eff es (S.Set V2)
getVisibleTiles = do
  pl <- getObject playerId
  let vs = fromMaybe (error "implement some proper fucking object tagging you dumb cunt") $ getViewshedMaybe pl
  return (vs ^. #visibleTiles)

renderBottomTerminal ::
  State World :> es
  => IOE :> es
  => Eff es ()
renderBottomTerminal = do
  w <- get
  clearViewport (w ^. #viewports % #bottomViewport)
  withViewport (w ^. #viewports % #bottomViewport) $ do
    borderViewport (Colour 0xFF999999) unicodeBorders
    viewportDrawTile (V2 4 5) Nothing (Colour 0xFF6644AA) '?'
  clearViewport (w ^. #viewports % #sideViewport)
  withViewport (w ^. #viewports % #sideViewport) $ do
    borderViewport (Colour 0xFF999999) unicodeBorders
    viewportDrawTile (V2 3 3) Nothing (Colour 0xFF6644AA) '!'

renderMap ::
  State World :> es
  => IOE :> es
  => S.Set V2
  -> Eff es ()
renderMap vt = do
  w <- get
  clearViewport (w ^. #viewports % #mapViewport)
  let es = w ^. #tileMap
  traverseArrayWithCoord_ (es ^. #revealedTiles) $ \p rev -> when rev $ whenInViewport (w ^. #viewports % #mapViewport) p $ do
    t <- use $ tile p
    let r = t ^. #renderable
    terminalColour (desaturate $ toGreyscale $ r ^. #foreground)
    terminalBkColour (desaturate $ toGreyscale $ r ^. #background)
    void $ withV2 p terminalPrintText (one $ r ^. #glyph)
  forM_ vt $ \a -> whenInViewport (w ^. #viewports % #mapViewport) a $ do
    t <- use $ tile a
    let r = t ^. #renderable
    terminalColour (r ^. #foreground)
    terminalBkColour (r ^. #background)
    --if (w ^. #tileMap % #walkableTiles) !@ a then terminalBkColour (Colour 0xFF0000FF) else terminalBkColour (Colour 0xFFFFFF44)
    void $ withV2 a terminalPrintText (one $ r ^. #glyph)

renderObjects ::
  State World :> es
  => IOE :> es
  => S.Set V2
  -> Eff es ()
renderObjects vt = do
  es <- use #objects
  forM_ es $ \v -> when ((v ^. #position) `S.member` vt) $ do
    let r = v ^. #renderable
    terminalColour (r ^. #foreground)
    terminalBkColour (r ^. #background)
    terminalPrintText (v ^. #position % _1) (v ^. #position % _2) (one $ r ^. #glyph)
    pass

everyTurn ::
  Breadcrumbs :> es => IOE :> es => State Metadata :> es => ObjectQuery Object :> es
  => State World :> es
  => Eff es ()
everyTurn = do
  tickTurn
  updateViewsheds
  monstersThink

monstersThink :: Breadcrumbs :> es => IOE :> es => State Metadata :> es => ObjectQuery Object :> es => State World :> es => Eff es ()
monstersThink = do
  p <- position <$> getObject (Entity 0)
  traverseObjects $ \o -> do
    if objectType o == "monster" then do
      whenJust (getViewshedMaybe o) $ \v -> do
        when (p `S.member` visibleTiles v) $ do
          print $ view #name o <> " yells insults at you"
          m <- use @World #tileMap
          print (V.length $ V.filter id $ toVector $ m ^. #walkableTiles)
          r <- findPath m (position o) p
          print r
          case r of
            Just (nextStep:_:_) -> void $ runRulebook Nothing moveRulebook (MoveArguments o (directionFromPoints (position o) nextStep))
            _ -> pass
      return Nothing
    else return Nothing

directionFromPoints :: V2 -> V2 -> Direction
directionFromPoints loc dest = case dest - loc of
  V2 (-1) 0 -> LeftDir
  V2 1 0 -> RightDir
  V2 0 1 -> DownDir
  V2 0 (-1) -> UpDir
  V2 (-1) (-1) -> UpLeftDir
  V2 (-1) 1 -> DownLeftDir
  V2 1 1 -> DownRightDir
  V2 1 (-1) -> UpRightDir
  _ -> error "invalid"
