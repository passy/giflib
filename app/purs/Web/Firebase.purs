module Web.Firebase where

import Control.Monad.Eff (Eff())
import Web.Firebase.Types (Firebase(), FirebaseEff())
import Web.Giflib.Types (URI()) -- TODO: Don't cross-depend here


foreign import newFirebase """
  function newFirebase(uri) {
    return new Firebase(uri);
  }
""" :: forall eff. URI -> Eff (firebase :: FirebaseEff | eff) Firebase
