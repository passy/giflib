-- | Hopefully this should become obsolete as soon as Halogen re-lands
--   CSS support.

module Web.Giflib.HTML.CSS.Unsafe
( style
) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Css.Stylesheet (StyleM())
import Halogen.HTML.Core (Prop(..), prop, propName, attrName, runClassName)

import qualified Css.Render as CSS

render :: forall a. StyleM a -> String
render = CSS.render >>> CSS.renderedInline >>> fromJust

inlineStyle :: forall i. String -> Prop i
inlineStyle = prop (propName "style") (Just $ attrName "style")

style :: forall a i. StyleM a -> Prop i
style = render >>> inlineStyle
