module Main exposing (..)

import Html
import Html exposing (program, Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List.Extra as LE

type alias Point = 
  { x : Float
  , y : Float
  , active : Bool
  , id : Int
  }

type alias Model = 
  { data : List Point
  , x_axis : Axis
  , y_axis : Axis
  }

type alias Axis =
  { max_extent : Float
  , min_extent : Float
  , max_value : Float
  , min_value : Float
  }

type Msg
  = SwitchPoint Point
  | None

initialModel : Model
initialModel =
    { data = [ {x = 10, y = 20, active = True, id = 1}
             , {x = 20, y = 30, active = False, id = 2}
             , {x = 30, y = 40, active = True, id = 3}
             ]
    , x_axis = Axis 120 0 30 10
    , y_axis = Axis 120 0 20 40
    }

max_x : Model -> Float
max_x model =
  case LE.maximumBy .x model.data of
    Just point -> 
      point.x
    Nothing ->  120

axisTransform : Axis -> Float -> Float
axisTransform axis value =
  (axis.max_value - axis.min_value)/(axis.max_extent - axis.min_extent) * value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
      ( model ! [])

    SwitchPoint point ->
      let
        _ = Debug.log "clicked" point
        -- remove current point
        -- invert the active flag
        -- re-add to list
        newData = model.data
      in
      ( { model | data = newData } ! [])

viewBox_ : Model -> String
viewBox_ model = 
  String.concat ["0 0 ", (toString model.x_axis.max_extent),  " ",  (toString model.y_axis.max_extent)]

dots : Point -> Svg Msg
dots point = 
  let
      color = case point.active of
                True ->
                  "black"
                False ->
                  "grey"
  in
    circle 
      [ cx (toString point.x)
      , cy (toString point.y)
      , r "5" 
      , stroke color
      , fill color
      , onClick (SwitchPoint point)
      ] []

view : Model -> Html Msg
view model =
  svg
    [ width (toString model.x_axis.max_extent), height (toString model.y_axis.max_extent), viewBox (viewBox_ model) ]
    (List.map dots model.data)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : ( Model, Cmd Msg)
init = 
    ( initialModel, Cmd.none)

main : Program Never Model Msg
main = 
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
