module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    (Model 0) ! []



-- Model


type alias Model =
    { attribute : Int
    }



-- Update


type Msg
    = IncrementAttribute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementAttribute ->
            { model | attribute = model.attribute + 1 } ! []



-- View


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.attribute)
        , button [ onClick IncrementAttribute ] [ text "+" ]
        ]
