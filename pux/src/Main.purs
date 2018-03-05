module Main where

import Prelude (Unit, unit, ($), bind, discard, (*>), pure, (<>), (==), show, (<$>), mod, (*), id, map, (<<<), flip, not)
import Data.Int (floor)
import Data.Array (snoc, deleteAt, (!!), length, modifyAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.TraversableWithIndex (forWithIndex)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (delay)

import Pux (start, EffModel, noEffects, CoreEffects)
import Pux.Renderer.React (renderToDOM)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes as HA
import Pux.DOM.Events as HE
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (text, (!), (#!))
import DOM (DOM)
import DOM.Event.Event (preventDefault)

data State = State
    { items :: Array Todo
    , notification :: Maybe String
    , newItemName :: String
    }

data Todo = Todo
    { name :: String
    , completed :: Boolean
    }

data Event
    = ChangeNewItemName HE.DOMEvent
    | AddNewItem HE.DOMEvent
    | RemoveItem HE.DOMEvent Int
    | SetCompleted HE.DOMEvent Int Boolean
    | SetNotification (Maybe String)

list :: State -> HTML Event
list (State st) =
    H.div do
       H.h1 $ text "TODO app"
       notification $ st.notification
       itemList st.items
       newItem st.newItemName

notification :: Maybe String -> HTML Event
notification n =
    H.div ! HA.className (notificationClassName n) $ text $ fromMaybe "" n
    where
          notificationClassName m = if isJust m then "notification active" else "notification passive"

itemList :: Array Todo -> HTML Event
itemList todos = H.ul $ forWithIndex todos todoItem *> pure unit

todoItem :: Int -> Todo -> HTML Event
todoItem index (Todo todo) =
    H.div ! HA.key (show index) $ do
        H.span ! HA.className "name" $ text todo.name
        H.input ! HA.type' "checkbox" ! HA.checked (checkedValue todo.completed) #! HE.onClick (\ev -> SetCompleted ev index $ not todo.completed)
        H.a ! HA.className "removeItem" #! HE.onClick (\ev -> RemoveItem ev index) $ text "remove"
    where
        checkedValue :: Boolean -> String
        checkedValue b = if b then "checked" else ""

newItem :: String -> HTML Event
newItem value =
    H.div ! HA.className "new-item" $ do
        H.input ! HA.placeholder "new item name" ! HA.value value #! HE.onChange ChangeNewItemName
        H.button #! HE.onClick AddNewItem $ text "Add new item"

foldp :: forall fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (ChangeNewItemName ev) (State st) = withPreventDefault ev $ noEffects $ State $ st { newItemName = HE.targetValue ev }
foldp (AddNewItem ev) state@(State st) = withNotification st.newItemName $ withPreventDefault ev $ noEffects $ modifyItems (appendNewTodo st.newItemName) state
foldp (RemoveItem ev index) state = withPreventDefault ev $ noEffects $ modifyItems (deleteAt index) state
foldp (SetCompleted ev index completed) st = noEffects $ modifyItems (modifyAt index (setCompleted completed)) st
foldp (SetNotification notification) st = noEffects $ setNotification notification st

modifyItems :: (Array Todo -> Maybe (Array Todo)) -> State -> State
modifyItems updater (State st) = State st { items = fromMaybe st.items $  updater st.items }

setNewItemName :: String -> State -> State
setNewItemName newItemName (State st) = State st { newItemName = newItemName }

appendNewTodo :: String -> Array Todo -> Maybe (Array Todo)
appendNewTodo name todos = if name == "" then Nothing else Just $ snoc todos $ Todo { name: name, completed: false }

setCompleted :: Boolean -> Todo -> Todo
setCompleted completed (Todo todo) = Todo todo { completed = completed }

setNotification :: Maybe String -> State -> State
setNotification notification (State st) = State st { notification = notification }

withNotification :: forall fx. String -> EffModel State Event (dom :: DOM | fx) -> EffModel State Event (dom :: DOM | fx)
withNotification notification { state, effects } = { state: state, effects: effects <> [ pure $ Just $ SetNotification $ Just notification
    , do
    delay $ Milliseconds 2000.0
    pure $ Just $ SetNotification Nothing
    ] }

withPreventDefault :: forall fx. HE.DOMEvent -> EffModel State Event (dom :: DOM | fx) -> EffModel State Event (dom :: DOM | fx)
withPreventDefault ev { state, effects } = { state: state, effects: effects <> [liftEff (preventDefault ev) *> pure Nothing] }

init :: State
init = State
    { items:
        [ Todo { name: "learn typescript", completed: false }
        , Todo { name: "fix handbrake", completed: false }
        ]
    , notification: Nothing
    , newItemName: ""
    }

main :: forall e. Eff (CoreEffects (dom :: DOM | e)) Unit
main = do
    app <- start
        { initialState: init
        , view: list
        , foldp
        , inputs: []
        }
    renderToDOM "#root" app.markup app.input
