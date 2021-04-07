module Main where

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import           Brick.Util           (bg, on)
import qualified Brick.Widgets.Dialog as D
import           Data.Monoid
import qualified Graphics.Vty         as V
import           Gui
import           Lens.Micro
import qualified State                as S

appEvent :: S.State -> T.BrickEvent () e -> T.EventM () (T.Next S.State)
appEvent s (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc []        -> M.halt s
    V.EvKey V.KEnter []      -> M.halt s
    V.EvKey V.KLeft []       -> M.continue changeButton
    V.EvKey V.KRight []      -> M.continue changeButton
    V.EvKey (V.KChar ' ') [] -> M.continue handleCounter
    _                        -> M.continue s
  where
    changeButton = s & S.isIncrement %~ not
    handleCounter = s & S.count %~ fn
      where
        fn = if s ^. S.isIncrement then (+ 1) else subtract 1
appEvent s _ = M.continue s

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(D.buttonAttr, V.white `on` V.blue), (D.buttonSelectedAttr, bg V.yellow)]

theApp :: M.App S.State e ()
theApp =
  M.App
    { M.appDraw = drawGui,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  finalState <- M.defaultMain theApp S.getInitialState
  print $ finalState ^. S.count
