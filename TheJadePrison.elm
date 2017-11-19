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


type alias CreationManager =
    { attributePoints : Int
    , abilityPoints : Int
    , favouredAbilities : Int
    }


type alias PlayerInformation =
    Dict.Dict String (Maybe String)


type alias ExAttributes =
    Dict.Dict String Int


type alias ExAbilities =
    Dict.Dict String ( Bool, Int )


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


emptyExAbilities : ExAbilities
emptyExAbilities =
    Dict.fromList
        [ ( "Archery", ( False, 0 ) )
        , ( "Athletics", ( False, 0 ) )
        , ( "Awareness", ( False, 0 ) )
        , ( "Brawl", ( False, 0 ) )
        , ( "Bureaucracy", ( False, 0 ) )
        , ( "Craft", ( False, 0 ) )
        , ( "Dodge", ( False, 0 ) )
        , ( "Integrity", ( False, 0 ) )
        , ( "Investigation", ( False, 0 ) )
        , ( "Larceny", ( False, 0 ) )
        , ( "Linguistics", ( False, 0 ) )
        , ( "Lore", ( False, 0 ) )
        , ( "Martial Arts", ( False, 0 ) )
        , ( "Medicine", ( False, 0 ) )
        , ( "Melee", ( False, 0 ) )
        , ( "Occult", ( False, 0 ) )
        , ( "Performance", ( False, 0 ) )
        , ( "Presence", ( False, 0 ) )
        , ( "Resistance", ( False, 0 ) )
        , ( "Ride", ( False, 0 ) )
        , ( "Sail", ( False, 0 ) )
        , ( "Socialize", ( False, 0 ) )
        , ( "Stealth", ( False, 0 ) )
        , ( "Survival", ( False, 0 ) )
        , ( "Thrown", ( False, 0 ) )
        , ( "War", ( False, 0 ) )
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


updateExAttributes :
    ExAttributes
    -> String
    -> Int
    -> CreationManager
    -> ( ExAttributes, CreationManager )
updateExAttributes attributes exAttribute newValue creationManager =
    let
        currentValue =
            Dict.get exAttribute attributes
                |> Maybe.withDefault 1

        atValue =
            if (newValue == currentValue) then
                1
            else
                newValue

        updatedAttributePoints =
            creationManager.attributePoints - (atValue - currentValue)

        newCM =
            { creationManager
                | attributePoints =
                    updatedAttributePoints
            }
    in
        ( Dict.insert exAttribute atValue attributes, newCM )


updateExAbilities :
    ExAbilities
    -> String
    -> Int
    -> CreationManager
    -> ( ExAbilities, CreationManager )
updateExAbilities exAbilities exAbility newValue creationManager =
    let
        ( favoured, currentValue ) =
            Dict.get exAbility exAbilities
                |> Maybe.withDefault ( False, 0 )

        abValue =
            if (newValue == currentValue) then
                0
            else
                newValue

        updatedValue =
            ( favoured, abValue )

        updatedAbilityPoints =
            creationManager.abilityPoints - (abValue - currentValue)

        newCM =
            { creationManager
                | abilityPoints =
                    updatedAbilityPoints
            }
    in
        ( Dict.insert exAbility updatedValue exAbilities, newCM )


toggleCasteOrFavoured :
    String
    -> ExAbilities
    -> CreationManager
    -> ( ExAbilities, CreationManager )
toggleCasteOrFavoured exAbility exAbilities creationManager =
    let
        ( favoured, abilityValue ) =
            Dict.get exAbility exAbilities
                |> Maybe.withDefault ( False, 0 )

        newCM =
            { creationManager
                | favouredAbilities =
                    if not favoured then
                        creationManager.favouredAbilities - 1
                    else
                        creationManager.favouredAbilities + 1
            }

        toggledFavoured =
            ( not favoured, abilityValue )
    in
        ( Dict.insert exAbility toggledFavoured exAbilities, newCM )



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
            , allExAttributesView model
            , allAbilitiesView model
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



-- Attributes Section


attributes : List String
attributes =
    physicalAttributes ++ socialAttributes ++ mentalAttributes


physicalAttributes : List String
physicalAttributes =
    [ "Strength"
    , "Dexterity"
    , "Stamina"
    ]


socialAttributes : List String
socialAttributes =
    [ "Charisma"
    , "Manipulation"
    , "Appearance"
    ]


mentalAttributes : List String
mentalAttributes =
    [ "Perception"
    , "Intelligence"
    , "Wits"
    ]


allExAttributesView : Model -> Html Msg
allExAttributesView model =
    let
        cmAttributes =
            attributesSection
                model.creationManager
                model.exAttributes
    in
        div [ class "attributes" ]
            [ div [ class "title-box-3col" ] [ h2 [] [ text "Attributes" ] ]
            , cmAttributes "Physical" physicalAttributes
            , cmAttributes "Mental" socialAttributes
            , cmAttributes "Social" mentalAttributes
            ]


attributesSection :
    CreationManager
    -> ExAttributes
    -> String
    -> List String
    -> Html Msg
attributesSection creationManager exAttributes sectionName sectionAttributes =
    div []
        ([ h3 [] [ text sectionName ] ]
            ++ (List.map
                    (exAttributeView exAttributes creationManager)
                    sectionAttributes
               )
        )


exAttributeView : ExAttributes -> CreationManager -> String -> Html Msg
exAttributeView exAttributes creationManager exAttribute =
    let
        exAttributeVal =
            Dict.get exAttribute exAttributes
                |> Maybe.withDefault 1

        filledList =
            List.map2 (\ref val -> ref >= val)
                (List.repeat 5 exAttributeVal)
                (List.range 1 5)

        overSpent =
            if creationManager.attributePoints < 0 then
                True
            else
                False
    in
        div []
            [ text exAttribute
            , div
                []
                (List.map2 (attributeDot exAttribute overSpent)
                    (List.range 1 5)
                    filledList
                )
            ]


attributeDot : String -> Bool -> Int -> Bool -> Html Msg
attributeDot exAttribute overSpent attributeValue filled =
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
                if overSpent then
                    SvgAtt.fill "red"
                else
                    SvgAtt.fill "black"
              else
                SvgAtt.fill "white"
            ]
            []
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


allAbilitiesView : Model -> Html Msg
allAbilitiesView model =
    div [ class "abilities" ]
        [ div [ class "title-box-3col" ] [ h2 [] [ text "Abilities" ] ]
        , div []
            (List.map
                (exAbilityView model.exAbilities model.creationManager)
                abilities
            )
        ]


exAbilityView : ExAbilities -> CreationManager -> String -> Html Msg
exAbilityView exAbilities creationManager exAbility =
    let
        ( favoured, exAbilityVal ) =
            Dict.get exAbility exAbilities
                |> Maybe.withDefault ( False, 0 )

        filledList =
            List.map2 (\ref val -> ref >= val)
                (List.repeat 5 exAbilityVal)
                (List.range 1 5)

        overSpent =
            if creationManager.abilityPoints < 0 then
                True
            else
                False

        maxFavoured =
            if creationManager.favouredAbilities < 0 then
                True
            else
                False
    in
        div []
            [ casteOrFavouredBox exAbility favoured maxFavoured
            , text exAbility
            , div
                []
                (List.map2 (abilityDot exAbility overSpent)
                    (List.range 1 5)
                    filledList
                )
            ]


abilityDot : String -> Bool -> Int -> Bool -> Html Msg
abilityDot exAbility overSpent abilityValue filled =
    Svg.svg
        [ SvgAtt.width "20"
        , SvgAtt.height "20"
        , onClick (EditExAbility exAbility abilityValue)
        ]
        [ Svg.circle
            [ SvgAtt.cx "10"
            , SvgAtt.cy "10"
            , SvgAtt.r "8"
            , SvgAtt.stroke "black"
            , SvgAtt.strokeWidth "2"
            , if filled then
                if overSpent then
                    SvgAtt.fill "red"
                else
                    SvgAtt.fill "black"
              else
                SvgAtt.fill "white"
            ]
            []
        ]


casteOrFavouredBox : String -> Bool -> Bool -> Html Msg
casteOrFavouredBox exAbility casteOrFavoured maxFavoured =
    Svg.svg
        [ SvgAtt.width "18"
        , SvgAtt.height "18"
        , onClick (ToggleCasteOrFavoured exAbility)
        ]
        [ Svg.rect
            [ SvgAtt.width "18"
            , SvgAtt.height "18"
            , SvgAtt.stroke "black"
            , SvgAtt.strokeWidth "4"
            , if casteOrFavoured then
                if maxFavoured then
                    SvgAtt.fill "red"
                else
                    SvgAtt.fill "black"
              else
                SvgAtt.fill "white"
            ]
            []
        ]
