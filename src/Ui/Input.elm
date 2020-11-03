module Ui.Input exposing (text)

import Html
import Html.Attributes
import Element.Border as Border
import Element.Input as Input
import Element as El exposing (..)
import Element.Font as Font exposing (..)


text :
  List (Attribute msg) ->
    { onChange : String -> msg
    , text : String
    , placeholder : String
    , label : String
    , minWidth : Int
    , rightAlign : Bool
    }
    -> Element msg
text attributes config =
  row []
    [ html (Html.node "style" [] [Html.text ".s.imlp > .imlf { white-space: nowrap !important; }"])
    , Input.multiline
      ( [ borderBottom
        , Border.dashed
        , Border.color gray
        , if config.rightAlign then Font.alignRight else Font.alignLeft
        , if config.rightAlign then El.alignRight else El.alignLeft
        , htmlAttribute (Html.Attributes.style "text-align" (if config.rightAlign then "right" else "left"))
        , paddingXY 3 3
        , family [ typeface "LMRoman10-Regular", sansSerif ]
        ] ++ attributes
      )
      { onChange = config.onChange << String.filter (\c -> c /= '\n')
      , text = config.text
      , placeholder =
          Just <|
            Input.placeholder []
              (el
                [ Font.color gray
                , if config.rightAlign then El.alignRight else El.alignLeft
                ]
                (El.text config.placeholder)
              )
      , label = Input.labelHidden config.label
      , spellcheck = False
      }
    ]


gray : Color
gray =
  rgb255 175 175 175


borderBottom : Attribute msg
borderBottom =
  Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
