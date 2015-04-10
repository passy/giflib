module WSK.Button where

import Data.Monoid (mempty)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

data ButtonElevation = ButtonRaised | ButtonFlat

type Button = { elevation :: ButtonElevation
              , ripple    :: Boolean
              , text      :: String
              }

button :: forall p i. Button -> H.HTML p i
button t =
  H.button [ A.classes btnClasses ]
           [ H.text t.text ]
  where btnClasses =
    A.className <$> ([ "wsk-button"
                     , "wsk-js-button"
                     ] <> btnRaisedClass)
        btnRaisedClass = case t.elevation of
                              ButtonRaised -> pure $ "wsk-button--raised"
                              ButtonFlat   -> mempty
