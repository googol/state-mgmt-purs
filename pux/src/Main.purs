module Main where

import Prelude (Unit, ($), bind)
import Control.Monad.Eff (Eff)

import Pux (start, EffModel, noEffects, CoreEffects)
import Pux.Renderer.React (renderToDOM)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1)
import Text.Smolder.Markup (text)

data State = State { items :: Array Todo }

data Todo = Todo
    { name :: String
    , completed :: Boolean
    }

type Event = Unit

list :: State -> HTML Event
list (State st) =
    div do
       h1 $ text "TODO app"

foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp _ st = noEffects st

init :: State
init = State { items: [] }

main :: forall e. Eff (CoreEffects e) Unit
main = do
    app <- start
        { initialState: init
        , view: list
        , foldp
        , inputs: []
        }
    renderToDOM "#root" app.markup app.input
