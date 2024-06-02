module Main where

import TZRS.Prelude
import BearLibTerminal.Raw
import BearMonadTerminal
import qualified Data.Map.Strict as M

screenSize :: (Int, Int)
screenSize = (175, 35)

main :: IO ()
main = runEff $ evalStateShared defaultMetadata $ do
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
defaultMetadata = Metadata False (fst screenSize `div` 2, snd screenSize `div` 2)

data Metadata = Metadata
  { pendingQuit :: Bool
  , playerLocation :: Point
  } deriving stock (Generic, Show)

data Direction = LeftDir | RightDir | UpDir | DownDir

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

movePlayer ::
  State Metadata :> es
  => Direction
  -> Eff es ()
movePlayer = \case
  LeftDir -> #playerLocation % _1 %= flip (-) 1
  RightDir -> #playerLocation % _1 %= (+1)
  UpDir -> #playerLocation % _2 %= flip (-) 1
  DownDir -> #playerLocation % _2 %= (+1)

runLoop ::
  State Metadata :> es
  => IOE :> es
  => Eff es ()
runLoop = do
  terminalClear
  pl <- use #playerLocation
  terminalPrintText (pl ^. _1) (pl ^. _2) "@"
  terminalRefresh
  handleEvents Blocking $ \case
    Keypress kp -> do
      putStrLn $ "Handling keypress: " <> show kp
      case asMovement kp of
        Just mvDir -> movePlayer mvDir
        Nothing -> putStrLn ("unknown keypress: " <> show kp)
    WindowEvent Resize -> pass
    WindowEvent WindowClose -> #pendingQuit .= True
  ifM (use #pendingQuit) pass runLoop