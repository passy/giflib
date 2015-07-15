module Web.Giflib.Internal.Unsafe
  ( unsafePerformEff
  , unsafeShowPrintId
  , unsafePrintId
  , unsafeEvalEff
  , undefined
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Foreign (Foreign(), toForeign)
import Control.Monad.Eff.Console (log)


foreign import unsafePerformEff :: forall a eff. Eff eff a -> a

foreign import showForeign :: Foreign -> String

foreign import undefined :: forall a. a

unsafePrintId :: forall a. a -> a
unsafePrintId o = unsafePerformEff $ do
  log <<< showForeign <<< toForeign $ o
  return o

unsafeShowPrintId :: forall a. (Show a) => a -> a
unsafeShowPrintId o = unsafePerformEff $ do
  log <<< show $ o
  return o

-- | Run an effectful computation maintaining the type signature.
--   This can be helpful when passing callbacks to FFI functions,
--   but comes with obvious big scary risks.
foreign import unsafeEvalEff :: forall eff a. Eff eff a -> Eff eff a
