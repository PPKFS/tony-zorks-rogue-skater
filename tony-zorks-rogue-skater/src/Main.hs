module Main where

import TZRS.Prelude
import BearLibTerminal.Raw
import BearMonadTerminal

screenSize :: (Int, Int)
screenSize = (175, 35)

main :: IO ()
main = liftIO $ withWindow
  defaultWindowOptions { size = Just screenSize }
  (do
    terminalSetText "log: file='awa.log', level=trace;"
    terminalSetText "font: '../../../usr/share/fonts/TTF/IosevkaNerdFontMono-Medium.ttf', size=24"
  )
  (const runLoop)
  pass

runLoop :: MonadIO m => m ()
runLoop = do
  terminalClear
  terminalPrintText (fst screenSize `div` 2) (snd screenSize `div` 2) "@"
  terminalRefresh
  handleEvents Blocking $ \case
    Keypress kp -> putStrLn ("you pressed " <> show kp) >> return True
    WindowEvent Resize -> pass\
    WindowEvent WindowClose -> return False
  if and r then runLoop else pass