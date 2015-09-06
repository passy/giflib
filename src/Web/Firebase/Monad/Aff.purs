module Web.Firebase.Monad.Aff where

import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff())

import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB

import Web.Giflib.Internal.Unsafe (undefined)

on :: forall eff.
      FB.EventType ->
      FB.Firebase ->
      Aff (firebase :: FB.FirebaseEff | eff) FB.DataSnapshot
on etype fb = FB.on etype dataCb errCb fb
  where
    dataCb = undefined
    errCb = undefined
