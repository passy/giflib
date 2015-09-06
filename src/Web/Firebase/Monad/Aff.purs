module Web.Firebase.Monad.Aff where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception (error)

import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB

import Web.Giflib.Internal.Unsafe (undefined)

on :: forall eff.
      FB.EventType ->
      FB.Firebase ->
      Aff (firebase :: FB.FirebaseEff | eff) FB.DataSnapshot
on etype fb = makeAff (\eb cb -> FB.on etype cb Nothing fb)
  -- where
  --   onErr eb err =
  --     case err of
  --       Just _  -> eb $ error "Firebase Error (sorry, I can't tell you what went wrong)"
  --       Nothing -> pure unit
