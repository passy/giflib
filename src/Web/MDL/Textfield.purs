module MDL.Textfield where

import Prelude
import Data.Monoid
import Data.Maybe
import MDL.Internal (mip)
import Halogen.HTML.Properties.Indexed (I())

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

type Textfield = { type_         :: P.InputType
                 , id            :: Maybe String
                 , label         :: Maybe String
                 , floatingLabel :: Boolean
                 , value         :: String
                 }

defaultTextfield :: Textfield
defaultTextfield = { type_: P.InputText
                   , id: mempty
                   , label: mempty
                   , floatingLabel: true
                   , value: mempty
                   }

textfield :: forall p i.
  Array (P.IProp (P.InteractiveEvents (P.GlobalProperties (accept :: I, autocomplete :: I, autofocus :: I, checked :: I, disabled :: I, form :: I, formaction :: I, formenctype :: I, formmethod :: I, formnovalidate :: I, formtarget :: I, height :: I, list :: I, max :: I, min :: I, multiple :: I, onAbort :: I, onChange :: I, onError :: I, onInput :: I, onInvalid :: I, onLoad :: I, onSearch :: I, onSelect :: I, pattern :: I, placeholder :: I, readonly :: I, required :: I, size :: I, src :: I, step :: I, inputType :: I, value :: I, width :: I))) i)
  -> Textfield
  -> H.HTML p i
textfield attrs t =
  H.div [ P.classes mainClasses ]
    ([ H.input ([ P.class_ clsTextfieldInput
                , P.inputType t.type_
                ] <> (mip P.id_ t.id) <> attrs)
     ] <> (mip (\lbl ->
       H.label ([ P.class_ clsLabel ] <> (mip P.for t.id)) [ H.text lbl ]) t.label)
    )
  where clsTextfield         = H.className "mdl-textfield"
        clsJsTextfield       = H.className "mdl-js-textfield"
        clsFloatingTextfield = H.className "mdl-textfield--floating-label"
        clsTextfieldInput    = H.className "mdl-textfield__input"
        clsLabel             = H.className "mdl-textfield__label"
        mainClasses =
          [ clsTextfield, clsJsTextfield ] ++
            if t.floatingLabel then [ clsFloatingTextfield ] else []

textfield_ :: forall p i. Textfield -> H.HTML p i
textfield_ = textfield mempty
