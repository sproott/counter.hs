module Core where

import           Brick.Types          (Widget)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (vBox)

centeredVBox :: [Widget n] -> Widget n
centeredVBox elements = vBox $ C.hCenter <$> elements
