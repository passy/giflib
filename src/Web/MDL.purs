module MDL
  ( card
  , cardImageContainer
  , cardHeading
  , cardHeadingText
  , cardCaption
  , cardBottom
  , cardUri
  , grid
  , shadow
  , cellCol
  )
where

import Prelude
import Data.String (joinWith)
import Halogen.HTML.Core (ClassName(), className)

card :: ClassName
card = className "mdl-card"

cardImageContainer :: ClassName
cardImageContainer = className "mdl-card--img-container"

cardHeading :: ClassName
cardHeading = className "mdl-card--heading"

cardHeadingText :: ClassName
cardHeadingText = className "mdl-card--heading-text"

cardCaption :: ClassName
cardCaption = className "mdl-card--caption"

cardBottom :: ClassName
cardBottom = className "mdl-card--bottom"

cardUri :: ClassName
cardUri = className "mdl-card--uri"

shadow :: Int -> ClassName
shadow z = className $ joinWith "" ["mdl-shadow--", show z, "dp"]

-- Grid classes

grid :: ClassName
grid = className "mdl-grid"

cell :: ClassName
cell = className "mdl-cell"

cellCol :: Int -> Array ClassName
cellCol n = [cell, className $ joinWith "" ["mdl-cell--", show n, "-col"]]
