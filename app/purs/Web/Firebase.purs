module Web.Firebase where

import Control.Monad.Eff (Eff())
import Web.Firebase.Types (Firebase(), FirebaseEff())
import Web.Giflib.Types (URI()) -- TODO: Don't cross-depend here


foreign import newFirebase """
  function newFirebase(uri) {
    return function () {
      return new Firebase(uri);
    };
  }
""" :: forall eff. URI -> Eff (firebase :: FirebaseEff | eff) Firebase

foreign import child """
  function child(childPath) {
    return function (firebase) {
      return function () {
        return firebase.child(childPath);
      };
    }
  }
""" :: forall eff. String -> Firebase -> Eff (firebase :: FirebaseEff | eff) Firebase

foreign import printVals """
    function printVals(fb) {
      return function () {
        fb.on('value', function (x) { console.log('fbval', x.val()); });
      }
    }
""" :: forall eff. Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
