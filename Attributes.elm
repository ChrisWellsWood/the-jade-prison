module Attributes
    exposing
        ( ExAttributes
        , emptyExAttributes
        , updateExAttributes
        , allExAttributesView
        )

import CreationManager exposing (CreationManager)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes as SvgAtt


type alias ExAttributes =
    Dict.Dict String Int


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



-- Attribute Views


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


allExAttributesView : ExAttributes -> CreationManager -> (String -> Int -> msg) -> Html msg
allExAttributesView exAttributes creationManager editMsg =
    let
        cmAttributes =
            attributesSection creationManager exAttributes editMsg
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
    ->
        (String
         -> Int
         -> msg
        )
    -> String
    -> List String
    -> Html msg
attributesSection creationManager exAttributes editMsg sectionName sectionAttributes =
    div []
        ([ h3 [] [ text sectionName ] ]
            ++ (List.map
                    (exAttributeView exAttributes creationManager editMsg)
                    sectionAttributes
               )
        )


exAttributeView :
    ExAttributes
    -> CreationManager
    ->
        (String
         -> Int
         -> msg
        )
    -> String
    -> Html msg
exAttributeView exAttributes creationManager editMsg exAttribute =
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
                (List.map2 (attributeDot editMsg exAttribute overSpent)
                    (List.range 1 5)
                    filledList
                )
            ]


attributeDot : (String -> Int -> msg) -> String -> Bool -> Int -> Bool -> Html msg
attributeDot editMsg exAttribute overSpent attributeValue filled =
    Svg.svg
        [ SvgAtt.width "20"
        , SvgAtt.height "20"
        , onClick (editMsg exAttribute attributeValue)
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
