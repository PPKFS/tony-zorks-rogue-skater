module TZRS.Viewshed where

import TZRS.Prelude
import TZRS.World
import TZRS.Entity
import Rogue.ObjectQuery
import TZRS.Object
import Rogue.FieldOfView.Visibility
import Rogue.FieldOfView.Raycasting
import qualified Data.Set as S
import Rogue.Array2D.Boxed ((//@))

makeViewshedDirty ::
  State World :> es
  => Entity
  -> Eff es ()
makeViewshedDirty v = #dirtyViewsheds %= (v:)

updateViewsheds :: ObjectQuery Object :> es => State World :> es => Eff es ()
updateViewsheds = do
  vs <- use #dirtyViewsheds
  traceShow ("viewsheds: " <> show (length vs)) pass
  #dirtyViewsheds .= []
  pass
  forM_ vs updateViewshed


updateViewshed :: ObjectQuery Object :> es => State World :> es => Entity -> Eff es ()
updateViewshed e = do
  o <- getObject e
  p <- use #player
  tm <- use #tileMap
  let v = getViewshedMaybe o
  whenJust v $ \v' -> do
    fov <- pure $ calculateFov tm (view #position o) (range v')
    modifyViewshed o (#visibleTiles .~ fov)
    --when (p == e) $ #tileMap % #revealedTiles %= (\rt -> rt //@ map (,True) (S.toList fov))
  --update it...
  pass