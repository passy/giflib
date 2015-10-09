module MDL.Spinner where

import Prelude
import Data.Monoid (mempty)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

spinner :: forall p i. Boolean -> H.HTML p i
spinner active =
  H.div ([ P.classes $
    H.className <$> [ "mdl-spinner" , "mdl-js-spinner" ] <> activeCls ])
    []
  where activeCls = if active
                      then pure "is-active"
                      else []
