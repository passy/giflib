module WSK.Textfield where

import Data.Monoid
import Data.Maybe
import WSK.Internal (mip)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

type Textfield = { type_         :: String
                 , id            :: Maybe String
                 , label         :: Maybe String
                 , floatingLabel :: Boolean
                 , value         :: String
                 }

defaultTextfield = { type_: "text"
                   , id: Nothing
                   , label: Nothing
                   , floatingLabel: true
                   , value: ""
                   }

textfield :: forall p i. Textfield -> H.HTML p i
textfield t =
  H.div [ A.classes mainClasses ]
    ([ H.input ([ A.class_ clsTextfieldInput
               , A.type_ t.type_
               ] <> (mip A.id_ t.id)) []
     ] <>
     (mip (\lbl -> H.label ([ A.class_ clsLabel ] <> (mip A.for t.id)) [ H.text lbl ]) t.label)
    )
  where clsTextfield         = A.className "wsk-textfield"
        clsJsTextfield       = A.className "wsk-js-textfield"
        clsFloatingTextfield = A.className "wsk-textfield--floating-label"
        clsTextfieldInput    = A.className "wsk-textfield__input"
        clsLabel             = A.className "wsk-textfield__label"
        mainClasses =
          [ clsTextfield, clsJsTextfield ] ++
            if t.floatingLabel then [ clsFloatingTextfield ] else []
