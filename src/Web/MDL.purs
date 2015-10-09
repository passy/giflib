module MDL
  ( card
  , cardTitle
  , cardTitleText
  , cardBorder
  , cardActions
  , cardSubtitleText
  , cardSupportingText
  , cardUri
  , grid
  , shadow
  , cellCol
  )
where

import Prelude
import Data.String (joinWith)
import Halogen.HTML.Core (ClassName())
import Halogen.HTML.Indexed (className)

card :: ClassName
card = className "mdl-card"

cardTitle :: ClassName
cardTitle = className "mdl-card__title"

cardTitleText :: ClassName
cardTitleText = className "mdl-card__title-text"

cardSubtitleText :: ClassName
cardSubtitleText = className "mdl-card__subtitle-text"

cardSupportingText :: ClassName
cardSupportingText = className "mdl-card__supporting-text"

cardBorder :: ClassName
cardBorder = className "mdl-card--border"

cardActions :: ClassName
cardActions = className "mdl-card__actions"

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
