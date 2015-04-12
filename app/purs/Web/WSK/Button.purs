module WSK.Button where

import Data.Maybe
import Data.Monoid
import WSK.Internal (mip)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

data ButtonElevation = ButtonRaised | ButtonFlat

type Button = { elevation :: ButtonElevation
              , id        :: Maybe String
              , ripple    :: Boolean
              , text      :: String
              }

defaultButton :: Button
defaultButton = { elevation: ButtonFlat
                , id: mempty
                , ripple: true
                , text: mempty
                }

button :: forall p i. Button -> H.HTML p i
button t =
  H.button ([ A.classes btnClasses ] <> (mip A.id_ t.id))
           [ H.text t.text ]
  where btnClasses =
    A.className <$> ([ "wsk-button"
                     , "wsk-js-button"
                     ] <> btnRaisedClass
                       <> btnRippleClass)
        btnRaisedClass = case t.elevation of
                              ButtonRaised -> pure "wsk-button--raised"
                              ButtonFlat   -> mempty
        btnRippleClass :: [String] -- Type unification fails otherwise.
        btnRippleClass = if t.ripple
                            then pure "wsk-js-ripple-effect"
                            else mempty
