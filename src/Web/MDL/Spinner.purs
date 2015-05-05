module MDL.Spinner where

import Data.Monoid (mempty)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

spinner :: forall p i. Boolean -> H.HTML p i
spinner active =
  H.div ([ A.classes $
    A.className <$> [ "mdl-spinner" , "mdl-js-spinner" ] <> activeCls ])
    []
  where activeCls = if active
                      then pure "is-active"
                      else []
