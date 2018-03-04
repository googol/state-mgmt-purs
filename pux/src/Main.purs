module Main where

import Prelude (Unit, ($), bind, discard)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (for_)
import Control.Monad.Eff (Eff)

import Pux (start, EffModel, noEffects, CoreEffects)
import Pux.Renderer.React (renderToDOM)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (text, (!))

data State = State
    { items :: Array Todo
    , notification :: Maybe String
    }

data Todo = Todo
    { name :: String
    , completed :: Boolean
    }

type Event = Unit

list :: State -> HTML Event
list (State st) =
    H.div do
       H.h1 $ text "TODO app"
       notification $ st.notification
       itemList st.items
       newItem

notification :: Maybe String -> HTML Event
notification n =
    H.div ! HA.className (notificationClassName n) $ text $ fromMaybe "" n
    where
          notificationClassName m = if isJust m then "notification active" else "notification passive"

itemList :: Array Todo -> HTML Event
itemList todos = H.ul $ for_ todos todoItem

todoItem :: Todo -> HTML Event
todoItem (Todo todo) =
    H.div do
        H.span ! HA.className "name" $ text todo.name
        H.input ! HA.type' "checkbox" ! HA.checked (checkedValue todo.completed)
        H.a ! HA.className "removeItem" $ text "remove"
    where
        checkedValue :: Boolean -> String
        checkedValue b = if b then "checked" else ""

newItem :: HTML Event
newItem =
    H.div ! HA.className "new-item" $ do
        H.input ! HA.placeholder "new item name"
        H.button $ text "Add new item"

foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp _ st = noEffects st

init :: State
init = State
    { items:
        [ Todo { name: "learn typescript", completed: false }
        , Todo { name: "fix handbrake", completed: false }
        ]
    , notification: Nothing
    }

main :: forall e. Eff (CoreEffects e) Unit
main = do
    app <- start
        { initialState: init
        , view: list
        , foldp
        , inputs: []
        }
    renderToDOM "#root" app.markup app.input
