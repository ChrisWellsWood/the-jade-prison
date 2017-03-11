module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    (Model "" "" "Dawn" "" "" "Archery") ! []



-- Model


type alias Model =
    { name : String
    , player : String
    , caste : String
    , concept : String
    , anima : String
    , supernalAbility : String
    }



-- Update


type Msg
    = EditName String
    | EditPlayer String
    | EditCaste String
    | EditConcept String
    | EditAnima String
    | EditSupernal String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditName name ->
            { model | name = name } ! []

        EditPlayer player ->
            { model | player = player } ! []

        EditCaste caste ->
            { model | caste = caste } ! []

        EditConcept concept ->
            { model | concept = concept } ! []

        EditAnima anima ->
            { model | anima = anima } ! []

        EditSupernal supernal ->
            { model | supernalAbility = supernal } ! []



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Name", onInput EditName ] []
        , input [ placeholder "Player", onInput EditPlayer ] []
        , casteSelect
        , br [] []
        , input [ placeholder "Concept", onInput EditConcept ] []
        , input [ placeholder "Anima", onInput EditAnima ] []
        , supernalSelect
        ]


casteSelect : Html Msg
casteSelect =
    select
        [ onInput EditCaste ]
        (List.map simpleOption castes)


simpleOption : String -> Html msg
simpleOption val =
    option [ value val ] [ text val ]


castes : List String
castes =
    [ "Dawn"
    , "Zenith"
    , "Twilight"
    , "Night"
    , "Eclipse"
    ]


supernalSelect : Html Msg
supernalSelect =
    select
        [ onInput EditSupernal ]
        (List.map simpleOption abilities)


abilities : List String
abilities =
    [ "Archery"
    , "Athletics"
    , "Awareness"
    , "Brawl"
    , "Bureaucracy"
    , "Craft"
    , "Dodge"
    , "Integrity"
    , "Investigation"
    , "Larceny"
    , "Linguistics"
    , "Lore"
    , "Martial Arts"
    , "Medicine"
    , "Melee"
    , "Occult"
    , "Performance"
    , "Presence"
    , "Resistance"
    , "Ride"
    , "Sail"
    , "Socialize"
    , "Stealth"
    , "Survival"
    , "Thrown"
    , "War"
    ]
