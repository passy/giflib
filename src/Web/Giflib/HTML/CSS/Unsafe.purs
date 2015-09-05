-- | Hopefully this should become obsolete as soon as Halogen re-lands
--   CSS support.

module HTML.CSS.Unsafe
( inlineStyle
, style
) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Css.Stylesheet (StyleM())
import Halogen.HTML.Core (Prop(..), prop, propName, attrName, runClassName)

import qualified Css.Render as CSS

render :: forall a. StyleM a -> String
render = CSS.render >>> CSS.renderedInline >>> fromJust

style :: forall i. String -> Prop i
style = prop (propName "style") (Just $ attrName "style")

inlineStyle :: forall a i. StyleM a -> Prop i
inlineStyle = render >>> style
