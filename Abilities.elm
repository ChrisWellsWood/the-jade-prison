module Abilities
    exposing
        ( ExAbilities
        , abilities
        , emptyExAbilities
        , updateExAbilities
        , toggleCasteOrFavoured
        , allAbilitiesView
        )

import CreationManager exposing (CreationManager)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes as SvgAtt


type alias ExAbilities =
    Dict.Dict String ( Bool, Int )


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



-- Abilities Section


type alias EditAbiMsg msg =
    String -> Int -> msg


type alias EditCOFMsg msg =
    String -> msg


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


allAbilitiesView :
    ExAbilities
    -> CreationManager
    -> EditAbiMsg msg
    -> EditCOFMsg msg
    -> Html msg
allAbilitiesView exAbilities creationManager editAbiMsg editCOFMsg =
    div [ class "abilities" ]
        [ div [ class "title-box-3col" ] [ h2 [] [ text "Abilities" ] ]
        , div []
            (List.map
                (exAbilityView exAbilities creationManager editAbiMsg editCOFMsg)
                abilities
            )
        ]


exAbilityView :
    ExAbilities
    -> CreationManager
    -> EditAbiMsg msg
    -> EditCOFMsg msg
    -> String
    -> Html msg
exAbilityView exAbilities creationManager editAbiMsg editCOFMsg exAbility =
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
            [ casteOrFavouredBox editCOFMsg exAbility favoured maxFavoured
            , text exAbility
            , div
                []
                (List.map2 (abilityDot editAbiMsg exAbility overSpent)
                    (List.range 1 5)
                    filledList
                )
            ]


abilityDot : EditAbiMsg msg -> String -> Bool -> Int -> Bool -> Html msg
abilityDot editAbiMsg exAbility overSpent abilityValue filled =
    Svg.svg
        [ SvgAtt.width "20"
        , SvgAtt.height "20"
        , onClick (editAbiMsg exAbility abilityValue)
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


casteOrFavouredBox : EditCOFMsg msg -> String -> Bool -> Bool -> Html msg
casteOrFavouredBox editCOFMsg exAbility casteOrFavoured maxFavoured =
    Svg.svg
        [ SvgAtt.width "18"
        , SvgAtt.height "18"
        , onClick (editCOFMsg exAbility)
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
