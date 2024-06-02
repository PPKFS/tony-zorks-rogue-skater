module Main where

import TZRS.Prelude
import TZRS.Object
import BearLibTerminal.Raw
import BearMonadTerminal
import qualified Data.Map.Strict as M
import TZRS.Store
import TZRS.Entity
import qualified Data.EnumMap as EM
import Foreign.C.Types (CUInt(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import System.Random

screenSize :: (Int, Int)
screenSize = (175, 35)

data World = World
  { objects :: Store Object
  , tileMap :: TileMap
  } deriving stock (Generic)

data TileInfo = TileInfo
  { renderable :: Renderable
  } deriving stock (Generic, Show)

data TileMap = TileMap
  { tileKinds :: IntMap TileInfo
  , tileMap :: V.Vector Int
  , dimensions :: (Int, Int)
  } deriving stock (Generic, Show)

data Metadata = Metadata
  { pendingQuit :: Bool
  } deriving stock (Generic, Show)

makeFieldLabelsNoPrefix ''World
makeFieldLabelsNoPrefix ''Metadata
main :: IO ()
main = runEff $
  evalStateShared defaultMetadata $ do
    w <- uncurry randomMap screenSize
    evalStateShared (World emptyStore w) $ do
      ((%=) @_ @World) (#objects % coerced) $ EM.insert playerId (Object
        { name = "player"
        , description = ""
        , objectId = playerId
        , objectType = ObjectKind "player"
        , creationTime = Timestamp 0
        , modifiedTime = Timestamp 0
        , position = Position 10 10
        , renderable = Renderable '@' (Colour 0xFFF33FFF) (Colour 0x00000000)
        , objectData = ObjectSpecifics
        })
      withWindow
        defaultWindowOptions { size = Just screenSize }
        (do
          terminalSetText "log: file='awa.log', level=trace;"
          terminalSetText "font: '../../../usr/share/fonts/TTF/IosevkaNerdFontMono-Medium.ttf', size=24"
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
  [ TileInfo (Renderable '.' (Colour 0xFF313036) (Colour 0x00000000))
  , TileInfo (Renderable '#' (Colour 0xFFb9caee) (Colour 0x00000000))

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

indexToCoord :: Int -> Int -> (Int, Int)
indexToCoord w i = (i `mod` w, i `div` w)
randomMap :: IOE :> es => Int -> Int -> Eff es TileMap
randomMap w h = do
  let (wallId, floorId) = (1, 0)
  let (wMax, hMax) = (w-1, h-1)
  randomWalls <- mapM (const $ randomRIO (0, (w*h)-1)) [(0::Int)..500]
  let is = IS.fromList randomWalls
  let v = V.generate (w*h)
        (\i ->
          case indexToCoord w i of
            (0, _) -> wallId
            (_, 0) -> wallId
            (x, y) -> if x == wMax || y == hMax || (i `IS.member` is) then wallId else floorId)
  return TileMap
    { tileKinds = defaultTileKinds
    , tileMap = v
    , dimensions = (w, h)
    }

movePlayer ::
  State World :> es
  => Direction
  -> Eff es ()
movePlayer dir = do
  #objects % at playerId % _Just % #position %=
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
    )

runLoop ::
  State World :> es
  => State Metadata :> es
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
        Just mvDir -> movePlayer mvDir
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
    let (x, y) = indexToCoord (es ^. #dimensions % _1) i
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
    terminalPrintText (v ^. #position % #x) (v ^. #position % #y) (one $ r ^. #glyph)
