module WSK.Textfield where

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

type Textfield = { type_       :: String
                 , placeholder :: String -- Should be a Maybe
                 }

textfield :: forall p r node. (H.HTMLRepr node) => Textfield -> node p r
textfield t =
  H.div [ A.classes [ clsTextfield, clsJsTextfield ] ]
    [ H.input [ A.class_ clsTextfieldInput
              , A.placeholder t.placeholder
              , A.type_ t.type_
              ] []
    ]
  where clsTextfield      = A.className "wsk-textfield"
        clsJsTextfield    = A.className "wsk-js-textfield"
        clsTextfieldInput = A.className "wsk-textfield__input"

