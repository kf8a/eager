module Main exposing (..)

import Graph exposing (..)
import Data exposing (..)
import Html
import Auth0


main : Program (Maybe Auth0.LoggedInUser) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
