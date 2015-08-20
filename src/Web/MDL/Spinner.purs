module MDL.Spinner where

import Prelude
import Data.Monoid (mempty)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.Properties as A

spinner :: forall i. Boolean -> H.HTML i
spinner active =
  H.div ([ A.classes $
    H.className <$> [ "mdl-spinner" , "mdl-js-spinner" ] <> activeCls ])
    []
  where activeCls = if active
                      then pure "is-active"
                      else []
