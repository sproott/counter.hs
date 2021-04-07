module Gui where

import qualified Brick.Main                 as M
import           Brick.Types                (Widget)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Brick.Widgets.Core         (hBox, padTopBottom, str, vBox,
                                             withAttr, withBorderStyle, (<+>),
                                             (<=>))
import qualified Brick.Widgets.Dialog       as D
import           Data.List                  (intersperse)
import           Lens.Micro                 ((&), (.~), (^.))
import qualified State                      as S

newtype Button = Button String deriving (Eq)

drawGui :: S.State -> [Widget ()]
drawGui s =
  [ C.center
      $   vBox
      $   C.hCenter
      <$> [ str "count: " <+> str (show $ s ^. S.count)
          , padTopBottom 2 $ hBox $ intersperse
            padding
            [drawButton "++" isIncrement, drawButton "--" (not isIncrement)]
          , str "Arrow keys to choose"
          , str "Space to press button"
          , str "Enter or Esc to exit"
          ]
  ]
 where
  padding     = str "   "
  isIncrement = s ^. S.isIncrement

drawButton :: String -> Bool -> Widget ()
drawButton text selected = withAttr attr $ str $ "  " <> text <> "  "
  where attr = if selected then D.buttonSelectedAttr else D.buttonAttr
