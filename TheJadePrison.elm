module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


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
    (Model emptyPlayerInfo emptyExAttributes) ! []



-- Model


type alias Model =
    { playerInformation : PlayerInformation
    , exAttributes : ExAttributes
    }


type alias PlayerInformation =
    Dict.Dict String (Maybe String)


type alias ExAttributes =
    Dict.Dict String Int


emptyPlayerInfo : PlayerInformation
emptyPlayerInfo =
    Dict.fromList
        [ ( "Name", Nothing )
        , ( "Player", Nothing )
        , ( "Caste", Just "Dawn" )
        , ( "Concept", Nothing )
        , ( "Anima", Nothing )
        , ( "Supernal", Just "Archery" )
        ]


emptyExAttributes : ExAttributes
emptyExAttributes =
    Dict.fromList
        [ ( "Strength", 1 )
        , ( "Dexterity", 1 )
        , ( "Stamina", 1 )
        , ( "Charisma", 1 )
        , ( "Manipulation", 1 )
        , ( "Appearance", 1 )
        , ( "Perception", 1 )
        , ( "Intelligence", 1 )
        , ( "Wits", 1 )
        ]



-- Update


type Msg
    = EditPlayerInformation String String
    | EditExAttribute Operation String


type Operation
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditPlayerInformation section val ->
            let
                newPlayerInfo =
                    Dict.insert
                        section
                        (if String.length val > 0 then
                            (Just val)
                         else
                            Nothing
                        )
                        model.playerInformation
            in
                { model | playerInformation = newPlayerInfo } ! []

        EditExAttribute operation exAttribute ->
            { model
                | exAttributes =
                    updateExAttributes
                        model.exAttributes
                        operation
                        exAttribute
            }
                ! []


updateExAttributes : ExAttributes -> Operation -> String -> ExAttributes
updateExAttributes attributes operation exAttribute =
    let
        oldAttribute =
            Dict.get exAttribute attributes
                |> Maybe.withDefault 0
    in
        case operation of
            Increment ->
                if oldAttribute < 5 then
                    Dict.insert
                        exAttribute
                        (oldAttribute + 1)
                        attributes
                else
                    attributes

            Decrement ->
                if oldAttribute > 1 then
                    Dict.insert
                        exAttribute
                        (oldAttribute - 1)
                        attributes
                else
                    attributes



-- View


view : Model -> Html Msg
view model =
    div []
        [ playerInformationView model
        , allExAttributesView model
        ]



-- Player Information Section


playerInformationView : Model -> Html Msg
playerInformationView model =
    div []
        [ input
            [ placeholder "Name"
            , onInput (EditPlayerInformation "Name")
            ]
            []
        , input
            [ placeholder "Player"
            , onInput (EditPlayerInformation "Player")
            ]
            []
        , casteSelect
        , br [] []
        , input
            [ placeholder "Concept"
            , onInput (EditPlayerInformation "Concept")
            ]
            []
        , input
            [ placeholder "Anima"
            , onInput (EditPlayerInformation "Anima")
            ]
            []
        , supernalSelect
        ]


casteSelect : Html Msg
casteSelect =
    select
        [ onInput (EditPlayerInformation "Caste") ]
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
        [ onInput (EditPlayerInformation "Supernal") ]
        (List.map simpleOption abilities)



-- Attributes Section


attributes : List String
attributes =
    [ "Strength"
    , "Dexterity"
    , "Stamina"
    , "Charisma"
    , "Manipulation"
    , "Appearance"
    , "Perception"
    , "Intelligence"
    , "Wits"
    ]


allExAttributesView : Model -> Html Msg
allExAttributesView model =
    div []
        (Dict.toList model.exAttributes
            |> List.map exAttributeView
        )


exAttributeView : ( String, Int ) -> Html Msg
exAttributeView ( exAttribute, exAttributeVal ) =
    div []
        [ text (exAttribute ++ " ")
        , text (toString exAttributeVal)
        , button
            [ onClick (EditExAttribute Decrement exAttribute) ]
            [ text "-" ]
        , button
            [ onClick (EditExAttribute Increment exAttribute) ]
            [ text "+" ]
        ]



-- Abilities Section


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
