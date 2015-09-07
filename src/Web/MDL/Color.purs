module MDL.Color
  ( color
  )
where

import Prelude
import Halogen.HTML.Core (ClassName(), className)


-- TODO: Would be great to import this and make it type-safe.
-- See https://github.com/google/material-design-lite/blob/cc4f0b0e5b036738176d1183cbef6ccbfd36f02a/src/_color-definitions.scss

color :: String -> ClassName
color s = className $ "mdl-color--" <> s
