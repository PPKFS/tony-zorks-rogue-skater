module Main where

import TZRS.Prelude
import TZRS.Object
import qualified Data.Map.Strict as M
import TZRS.Store
import TZRS.Entity
import qualified Data.EnumMap as EM
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
import Rogue.FieldOfView.Visibility
import Effectful.State.Dynamic
import Rogue.FieldOfView.Raycasting
import Rogue.ObjectQuery
import qualified Data.Set as S
import Effectful.Dispatch.Dynamic
import Rogue.Array2D.Boxed

screenSize :: V2
screenSize = V2 100 60

main :: IO ()
main = runEff $ runBreadcrumbs Nothing $
  evalStateShared defaultMetadata $ do
    w <- withV2 screenSize (roomMap screenSize)
    evalStateShared (World emptyStore w [] (Timestamp 0) (Entity 0)) $
      runQueryAsState $
      runTileMapAsState $ do
        pId <- generateEntity
        let player =
              (Object
                { name = "player"
                , description = ""
                , objectId = pId
                , objectType = ObjectKind "player"
                , creationTime = Timestamp 0
                , modifiedTime = Timestamp 0
                , position = V2 20 15
                , renderable = Renderable '@' (fromRGB 0x75 0xa2 0xeb ) (Colour 0x00000000)
                , objectData = PlayerSpecifics $ Player { viewshed = Viewshed S.empty 20}
                })
        setObject player
        withWindow
          defaultWindowOptions { size = Just screenSize }
          (do
            terminalSetText "log: file='awa.log', level=trace;"
            terminalSetText "font: 'Boxy.ttf', size=24"
          )
          (const runLoop)
          pass

defaultMetadata :: Metadata
defaultMetadata = Metadata False

data Direction = LeftDir | RightDir | UpDir | DownDir

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
  ti <- getTile newLoc
 {-} if walkable ti then rulePass else do
    traceShow "nope can't walk through a wall" pass
    return (Just False)-}
  rulePass

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


runQueryAsState ::
  State World :> es
  => Eff (ObjectQuery Object : es) a
  -> Eff es a
runQueryAsState = interpret $ \env -> \case
  GenerateEntity -> #entityCounter <<%= (+1)
  SetObject r -> #objects % at (getID r) %= updateIt r
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
  modifyObject e (#position %~
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
    ))
  #dirtyViewsheds %= (e:)

runLoop ::
  State World :> es
  => ObjectQuery Object :> es
  => State Metadata :> es
  => Breadcrumbs :> es
  => TileMap :> es
  => IOE :> es
  => Eff es ()
runLoop = do
  everyTurn
  terminalClear
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
  TileMap :> es
  => ObjectQuery Object :> es
  => State World :> es
  => IOE :> es
  => Eff es ()
renderMap = do
  es <- use #tileMap
  traverseArrayWithCoord_ (es ^. #revealedTiles) $ \p rev -> when rev $ do
    t <- getTile p
    let r = t ^. #renderable
    terminalColour (desaturate $ toGreyscale $ r ^. #foreground)
    terminalBkColour (desaturate $ toGreyscale $ r ^. #background)
    void $ withV2 p terminalPrintText (one $ r ^. #glyph)
  pl <- getObject playerId
  let vs = fromMaybe (error "") $ getViewshedMaybe pl
  forM_ (vs ^. #visibleTiles) $ \a -> do
    t <- getTile a
    let r = t ^. #renderable
    terminalColour (r ^. #foreground)
    terminalBkColour (r ^. #background)
    withV2 a terminalPrintText (one $ r ^. #glyph)

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

everyTurn ::
  ObjectQuery Object :> es
  => State World :> es
  => Eff es ()
everyTurn = do
  tickTurn
  updateViewsheds
  -- update all (dirty) viewsheds
  -- each AI entity should then see if it wants to act

updateViewsheds :: ObjectQuery Object :> es => State World :> es => Eff es ()
updateViewsheds = do
  vs <- use #dirtyViewsheds
  #dirtyViewsheds .= []
  forM_ vs updateViewshed

updateViewshed :: ObjectQuery Object :> es => State World :> es => Entity -> Eff es ()
updateViewshed e = do
  o <- getObject e
  tm <- use #tileMap
  let v = getViewshedMaybe o
  whenJust v $ \v' -> do
    let fov = calculateFov tm (view #position o) (range v')
    modifyViewshed o (#visibleTiles .~ fov)
    #tileMap % #revealedTiles %= (\rt -> rt //@ map (,True) (S.toList fov))
  --update it...
  pass
