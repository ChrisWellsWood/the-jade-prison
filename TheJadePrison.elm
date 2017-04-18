module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Svg
import Svg.Attributes as SvgAtt


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
    | EditExAttribute String Int


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

        EditExAttribute exAttribute attributeValue ->
            { model
                | exAttributes =
                    updateExAttributes
                        model.exAttributes
                        exAttribute
                        attributeValue
            }
                ! []


updateExAttributes : ExAttributes -> String -> Int -> ExAttributes
updateExAttributes attributes exAttribute attributeValue =
    Dict.insert exAttribute attributeValue attributes



-- View


view : Model -> Html Msg
view model =
    div []
        [ playerInformationView model
        , allExAttributesView model
        ]


pointDot : String -> Int -> Bool -> Html Msg
pointDot exAttribute attributeValue filled =
    Svg.svg
        [ SvgAtt.width "20"
        , SvgAtt.height "20"
        , onClick (EditExAttribute exAttribute attributeValue)
        ]
        [ Svg.circle
            [ SvgAtt.cx "10"
            , SvgAtt.cy "10"
            , SvgAtt.r "8"
            , SvgAtt.stroke "black"
            , SvgAtt.strokeWidth "2"
            , if filled then
                  SvgAtt.fill "black"
              else
                  SvgAtt.fill "white"
            ]
            []
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
    let
        filledList =
            List.map2 (\ref val -> ref >= val)
                (List.repeat 5 exAttributeVal)
                (List.range 1 5)
    in
        div []
            [ text exAttribute
            , div
                []
                ( List.map2 (pointDot exAttribute)
                    (List.range 1 5)
                    filledList 
                )
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
