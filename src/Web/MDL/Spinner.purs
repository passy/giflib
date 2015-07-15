module MDL.Spinner where

import Prelude
import Data.Monoid (mempty)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

spinner :: forall i. Boolean -> H.HTML i
spinner active =
  H.div ([ A.classes $
    A.className <$> [ "mdl-spinner" , "mdl-js-spinner" ] <> activeCls ])
    []
  where activeCls = if active
                      then pure "is-active"
                      else []
