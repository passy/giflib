module Thermite.Html.Attributes.Extra where

import qualified Thermite.Types as T
import Thermite.Internal (unsafeAttribute)

-- https://github.com/paf31/purescript-thermite/issues/18
style :: forall s action. { | s } -> T.Prop action
style = unsafeAttribute "style"
