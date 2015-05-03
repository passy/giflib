module Web.Giflib.Internal.Unsafe
  ( unsafePerformEff
  , unsafeShowPrintId
  , unsafePrintId
  , unsafeEvalEff
  , undefined
  ) where

import Control.Monad.Eff (Eff())
import Data.Foreign (Foreign(), toForeign)
import Debug.Trace (trace)


foreign import unsafePerformEff
  """
  function unsafePerformEff(f) {
    return f();
  }
  """ :: forall a eff. Eff eff a -> a

foreign import showForeign
  """
  function showForeign(a) {
    return JSON.stringify(a);
  }
  """ :: Foreign -> String

foreign import undefined :: forall a. a

unsafePrintId :: forall a. a -> a
unsafePrintId o = unsafePerformEff $ do
  trace <<< showForeign <<< toForeign $ o
  return o

unsafeShowPrintId :: forall a. (Show a) => a -> a
unsafeShowPrintId o = unsafePerformEff $ do
  trace <<< show $ o
  return o

-- | Run an effectful computation maintaining the type signature.
--   This can be helpful when passing callbacks to FFI functions,
--   but comes with obvious big scary risks.
foreign import unsafeEvalEff """
  function unsafeEvalEff(f) {
    f();
    return f;
  }
""":: forall eff a. Eff eff a -> Eff eff a
