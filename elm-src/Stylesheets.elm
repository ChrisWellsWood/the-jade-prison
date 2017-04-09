port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import JadePrisonCss


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
  Css.File.toFileStructure
    [ ( "style.css", Css.File.compile [ JadePrisonCss.css ] ) ]


main : CssCompilerProgram
main =
  Css.File.compiler files fileStructure