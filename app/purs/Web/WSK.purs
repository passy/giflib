module WSK where

import Halogen.HTML.Attributes (ClassName(), className)


card :: ClassName
card = className "wsk-card"

cardImageContainer :: ClassName
cardImageContainer = className "wsk-card--img-container"

cardHeading :: ClassName
cardHeading = className "wsk-card--heading"

cardHeadingText :: ClassName
cardHeadingText = className "wsk-card--heading-text"

cardCaption :: ClassName
cardCaption = className "wsk-card--caption"

cardBottom :: ClassName
cardBottom = className "wsk-card--buttom"

cardUri :: ClassName
cardUri = className "wsk-card--uri"

shadow :: Number -> ClassName
shadow z = className $ "wsk-shadow--z" ++ (show z)
