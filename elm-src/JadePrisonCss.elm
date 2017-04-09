module JadePrisonCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (body)


cssNamespace : String
cssNamespace = "jadePrison"


css : Stylesheet
css =
    ( stylesheet << namespace cssNamespace )
    [ body
        [ fontSize (px 12)
        ] 
    ]