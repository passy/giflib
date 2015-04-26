module Web.Firebase
( newFirebase
, child
, printVals
)
where

import Control.Monad.Eff (Eff())
import Web.Firebase.Types (Firebase(), FirebaseEff())
import Halogen.HTML.Target (URL(), runURL)
import Data.Function (Fn1(), runFn1)


foreign import newFirebaseImpl """
  function newFirebaseImpl(uri) {
    return function () {
      return new Firebase(uri);
    }
  }
""" :: forall eff. Fn1 String (Eff (firebase :: FirebaseEff | eff) Firebase)

newFirebase :: forall eff. URL -> Eff (firebase :: FirebaseEff | eff) Firebase
newFirebase u = runFn1 newFirebaseImpl $ runURL u

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
