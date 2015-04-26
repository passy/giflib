module Web.Firebase
( newFirebase
, child
, on
, EventType(..)
)
where

import Data.Maybe (Maybe())
import Data.Nullable (toNullable, Nullable())
import Control.Monad.Eff (Eff())
import Web.Firebase.Types (Firebase(), FirebaseEff(), FirebaseErr(), DataSnapshot())
import Halogen.HTML.Target (URL(), runURL)
import Data.Function (Fn1(), Fn2(), Fn4(), runFn1, runFn2, runFn4)
import Web.Giflib.Internal.Unsafe (unsafeEvalEff)


foreign import newFirebaseImpl """
  function newFirebaseImpl(uri) {
    return function () {
      return new Firebase(uri);
    }
  }
""" :: forall eff. Fn1 String (Eff (firebase :: FirebaseEff | eff) Firebase)

newFirebase :: forall eff. URL -> Eff (firebase :: FirebaseEff | eff) Firebase
newFirebase u = runFn1 newFirebaseImpl $ runURL u

foreign import childImpl """
  function childImpl(childPath, firebase) {
    return function () {
      return firebase.child(childPath);
    };
  }
""" :: forall eff. Fn2 String Firebase (Eff (firebase :: FirebaseEff | eff) Firebase)

child :: forall eff. String -> Firebase -> Eff (firebase :: FirebaseEff | eff) Firebase
child = runFn2 childImpl

data EventType = Value
               | ChildAdded
               | ChildChanged
               | ChildRemoved
               | ChildMoved

showEventType :: EventType -> String
showEventType t = case t of
                       Value -> "value"
                       ChildAdded -> "child_added"
                       ChildChanged -> "child_changed"
                       ChildRemoved -> "child_removed"
                       ChildMoved -> "child_moved"

foreign import onImpl """
  function onImpl(eventType, callback, cancelCallback, fb) {
    return function () {
      return fb.on(eventType, callback, cancelCallback);
    }
  }
""" :: forall eff. Fn4 String (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) (Nullable (FirebaseErr -> Eff eff Unit)) Firebase (Eff (firebase :: FirebaseEff | eff) Unit)

on :: forall eff.
      EventType ->
      (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) ->
      Maybe (FirebaseErr -> Eff eff Unit) ->
      Firebase ->
      Eff (firebase :: FirebaseEff | eff) Unit
on etype ds cb fb = runFn4 onImpl (showEventType etype) (unsafeEvalEff <<< ds) (toNullable cb) fb
