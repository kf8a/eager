module Main exposing (..)

import Graph exposing (..)
import Data exposing (..)
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }
