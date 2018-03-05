module Main where

import Prelude (($), (*), (*>), (<$>), (<>), (==), Unit, bind, discard, mod, not, pure, show, unit)
import Data.Int (floor)
import Data.Array ((!!), deleteAt, length, modifyAt, snoc)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.TraversableWithIndex (forWithIndex)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (delay)

import Pux (CoreEffects, EffModel, noEffects, start)
import Pux.Renderer.React (renderToDOM)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Pux.DOM.Events as HE
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((!), (#!), text)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import Signal.Time (every, second)

-- State
data State = State
    { lists :: Array TodoList
    , notification :: Maybe String
    , newListName :: String
    }

data TodoList = TodoList
    { items :: Array Todo
    , name :: String
    , newItemName :: String
    }

data Todo = Todo
    { name :: String
    , completed :: Boolean
    }

-- Events
type ListIndex = Int
type ItemIndex = Int
data Event
    = ChangeNewItemName HE.DOMEvent ListIndex
    | AddNewItem HE.DOMEvent ListIndex
    | RemoveItem HE.DOMEvent ListIndex ItemIndex
    | ChangeNewListName HE.DOMEvent
    | AddNewList HE.DOMEvent
    | RemoveList HE.DOMEvent ListIndex
    | AddReceivedItem (Maybe String)
    | SetCompleted HE.DOMEvent ListIndex ItemIndex Boolean
    | SetNotification (Maybe String)

-- Views
appView :: State -> HTML Event
appView (State st) =
    H.div do
       H.h1 $ text "TODO app"
       notification $ st.notification
       forWithIndex st.lists list *> pure unit
       newList st.newListName

notification :: Maybe String -> HTML Event
notification n =
    H.div ! HA.className (notificationClassName n) $ text $ fromMaybe "" n
    where
          notificationClassName m = if isJust m then "notification active" else "notification passive"

list :: ListIndex -> TodoList -> HTML Event
list index (TodoList todoList) =
    H.div ! key (show index) $ do
       H.h2 $ do
          text todoList.name
          H.a #! HE.onClick (\ev -> RemoveList ev index) $ text "remove list"
       itemList index todoList.items
       newItem index todoList.newItemName

itemList :: ListIndex -> Array Todo -> HTML Event
itemList listIndex todos = H.ul $ forWithIndex todos (todoItem listIndex) *> pure unit

todoItem :: ListIndex -> ItemIndex -> Todo -> HTML Event
todoItem listIndex index (Todo todo) =
    H.div ! key (show index) $ do
        H.span ! HA.className "name" $ text todo.name
        H.input ! HA.type' "checkbox" ! HA.checked (checkedValue todo.completed) #! HE.onClick (\ev -> SetCompleted ev listIndex index $ not todo.completed)
        H.a ! HA.className "removeItem" #! HE.onClick (\ev -> RemoveItem ev listIndex index) $ text "remove"
    where
        checkedValue :: Boolean -> String
        checkedValue b = if b then "checked" else ""

newList :: String -> HTML Event
newList name =
    H.div ! HA.className "new-list" $ do
        H.input ! HA.placeholder "new list name" ! HA.value name #! HE.onChange ChangeNewListName
        H.button #! HE.onClick AddNewList $ text "Add new list"

newItem :: ListIndex ->  String -> HTML Event
newItem listIndex value =
    H.div ! HA.className "new-item" $ do
        H.input ! HA.placeholder "new item name" ! HA.value value #! HE.onChange (\ev -> ChangeNewItemName ev listIndex)
        H.button #! HE.onClick (\ev -> AddNewItem ev listIndex) $ text "Add new item"

-- Handling of events
foldp :: forall fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (ChangeNewItemName ev listIndex) state = withPreventDefault ev $ noEffects $ modifyLists (modifyAt listIndex $ setNewItemName (HE.targetValue ev)) state
foldp (AddNewItem ev listIndex) state@(State st) = withNotification newItemName $ withPreventDefault ev $ noEffects $ modifyLists (modifyAt listIndex $ modifyItems $ appendNewTodo newItemName) state
    where
          newItemName = fromMaybe "" $ (\(TodoList tl) -> tl.newItemName) <$> (st.lists !! listIndex)
foldp (AddReceivedItem (Just name)) state = withNotification name $ noEffects $ modifyLists (modifyAt 0 $ modifyItems $ appendNewTodo name) state
foldp (AddReceivedItem Nothing) state = noEffects state
foldp (RemoveItem ev listIndex index) state = withPreventDefault ev $ noEffects $ modifyLists (modifyAt listIndex $ modifyItems $ deleteAt index) state
foldp (SetCompleted ev listIndex index completed) state = noEffects $ modifyLists (modifyAt listIndex $ modifyItems $ modifyAt index $ setCompleted completed) state
foldp (SetNotification notificationText) state = noEffects $ setNotification notificationText state
foldp (ChangeNewListName ev) state = withPreventDefault ev $ noEffects $ setNewListName (HE.targetValue ev) state
foldp (AddNewList ev) state@(State st) = withPreventDefault ev $ noEffects $ modifyLists (appendNewList st.newListName) state
foldp (RemoveList ev listIndex) state = withPreventDefault ev $ noEffects $ modifyLists (deleteAt listIndex) state

modifyLists :: (Array TodoList -> Maybe (Array TodoList)) -> State -> State
modifyLists updater (State st) = State st { lists = fromMaybe st.lists $  updater st.lists }

modifyItems :: (Array Todo -> Maybe (Array Todo)) -> TodoList -> TodoList
modifyItems updater (TodoList tl) = TodoList tl { items = fromMaybe tl.items $ updater tl.items }

setNewItemName :: String -> TodoList -> TodoList
setNewItemName newItemName (TodoList tl) = TodoList tl { newItemName = newItemName }

appendNewTodo :: String -> Array Todo -> Maybe (Array Todo)
appendNewTodo name todos = if name == "" then Nothing else Just $ snoc todos $ Todo { name: name, completed: false }

setCompleted :: Boolean -> Todo -> Todo
setCompleted completed (Todo todo) = Todo todo { completed = completed }

setNotification :: Maybe String -> State -> State
setNotification notificationText (State st) = State st { notification = notificationText }

setNewListName :: String -> State -> State
setNewListName newName (State st) = State st { newListName = newName }

appendNewList :: String -> Array TodoList -> Maybe (Array TodoList)
appendNewList newListName todoLists = if newListName == "" then Nothing else Just $ snoc todoLists $ TodoList { name: newListName, items: [], newItemName: "" }

withNotification :: forall fx. String -> EffModel State Event (dom :: DOM | fx) -> EffModel State Event (dom :: DOM | fx)
withNotification notificationText { state, effects } =
    { state: state
    , effects: effects <>
        [ pure $ Just $ SetNotification $ Just notificationText
        , do
            delay $ Milliseconds 2000.0
            pure $ Just $ SetNotification Nothing
        ]
    }

withPreventDefault :: forall fx. HE.DOMEvent -> EffModel State Event (dom :: DOM | fx) -> EffModel State Event (dom :: DOM | fx)
withPreventDefault ev { state, effects } = { state: state, effects: effects <> [liftEff (preventDefault ev) *> pure Nothing] }

init :: State
init = State
    { lists:
        [ TodoList
            { items:
                [ Todo { name: "learn typescript", completed: false }
                , Todo { name: "fix handbrake", completed: false }
                ]
            , name: "List 1"
            , newItemName: ""
            }
        ]
    , notification: Nothing
    , newListName: ""
    }

fakeServerResponses :: Array String
fakeServerResponses =
    [ "buy beer"
    , "clean the house"
    , "fix the bike"
    , "get a lawyer"
    , "buy a new shovel"
    , "dispose of the corpses in garage"
    , "get a haircut"
    , "shave more often"
    , "post something smart on twitter"
    , "understand monad transformers"
    , "blow up some things"
    , "have fun"
    , "visit new reaktor HQ"
    ]

getPseudorandomItem :: Number -> Maybe String
getPseudorandomItem n = fakeServerResponses !! (floor n) `mod` (length fakeServerResponses)

main :: forall e. Eff (CoreEffects (dom :: DOM | e)) Unit
main = do
    let fakeServer = AddReceivedItem <$> getPseudorandomItem <$> (every (10.0 * second))
    app <- start
        { initialState: init
        , view: appView
        , foldp
        , inputs: [ fakeServer ]
        }
    renderToDOM "#root" app.markup app.input
