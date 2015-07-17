module MDL
  ( card
  , cardImageContainer
  , cardHeading
  , cardHeadingText
  , cardCaption
  , cardBottom
  , cardUri
  , shadow
  )
where

import Prelude
import Halogen.HTML.Attributes (ClassName(), className)

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
shadow z = className $ "mdl-shadow--z" ++ (show z)
