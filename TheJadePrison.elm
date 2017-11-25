module Main exposing (..)

import Attributes exposing (..)
import Abilities exposing (..)
import CreationManager exposing (..)
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
    (Model
        (CreationManager 18 28 10)
        emptyPlayerInfo
        emptyExAttributes
        emptyExAbilities
    )
        ! []



-- Model


type alias Model =
    { creationManager : CreationManager
    , playerInformation : PlayerInformation
    , exAttributes : ExAttributes
    , exAbilities : ExAbilities
    }


type alias PlayerInformation =
    Dict.Dict String (Maybe String)


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



-- Update


type Msg
    = EditPlayerInformation String String
    | EditExAttribute String Int
    | EditExAbility String Int
    | ToggleCasteOrFavoured String


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
            let
                ( exAttributes, creationManager ) =
                    updateExAttributes
                        model.exAttributes
                        exAttribute
                        attributeValue
                        model.creationManager
            in
                { model
                    | exAttributes = exAttributes
                    , creationManager = creationManager
                }
                    ! []

        EditExAbility exAbility abilityValue ->
            let
                ( exAbilities, creationManager ) =
                    updateExAbilities
                        model.exAbilities
                        exAbility
                        abilityValue
                        model.creationManager
            in
                { model
                    | exAbilities = exAbilities
                    , creationManager = creationManager
                }
                    ! []

        ToggleCasteOrFavoured exAbility ->
            let
                ( exAbilities, creationManager ) =
                    toggleCasteOrFavoured
                        exAbility
                        model.exAbilities
                        model.creationManager
            in
                { model
                    | exAbilities = exAbilities
                    , creationManager = creationManager
                }
                    ! []



-- View


view : Model -> Html Msg
view model =
    div [ class "main-grid" ]
        [ div [ class "creation-manager" ]
            [ h2 [] [ text "Creation Progress" ]
            , text
                ("Attribute points: "
                    ++ toString model.creationManager.attributePoints
                    ++ "/18"
                )
            , hr [] []
            , text
                ("Ability points: "
                    ++ toString model.creationManager.abilityPoints
                    ++ "/28"
                )
            , hr [] []
            , text
                ("Caste or Favoured Abilities: "
                    ++ toString model.creationManager.favouredAbilities
                    ++ "/10"
                )
            ]
        , div [ class "character-sheet" ]
            [ playerInformationView model
            , allExAttributesView
                model.exAttributes
                model.creationManager
                EditExAttribute
            , allAbilitiesView
                model.exAbilities
                model.creationManager
                EditExAbility
                ToggleCasteOrFavoured
            ]
        ]



-- Player Information Section


playerInformationView : Model -> Html Msg
playerInformationView model =
    div [ class "player-info" ]
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
