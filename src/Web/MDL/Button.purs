module MDL.Button where

import Prelude
import Data.Maybe
import Data.Monoid
import MDL.Internal (mip)

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

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
  H.button ([ P.classes btnClasses ] <> (mip P.id_ t.id))
           [ H.text t.text ]
  where btnClasses = H.className <$> ([ "mdl-button"
                                      , "mdl-js-button"
                                      ] <> btnRaisedClass
                                        <> btnRippleClass)
        btnRaisedClass = case t.elevation of
                              ButtonRaised -> pure "mdl-button--raised"
                              ButtonFlat   -> mempty
        btnRippleClass :: (Array String) -- Type unification fails otherwise.
        btnRippleClass = if t.ripple
                            then pure "mdl-js-ripple-effect"
                            else mempty
