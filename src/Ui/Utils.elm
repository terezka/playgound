module Ui.Utils exposing (..)

import Browser.Dom as Dom
import Element as El exposing (..)
import Element.Border as Border
import Element.Font as Font exposing (..)
import Html.Attributes
import Task


id : String -> Attribute msg
id string =
  htmlAttribute (Html.Attributes.id string)


paddingTRBL : Int -> Int -> Int -> Int -> Attribute msg
paddingTRBL top right bottom left =
  paddingEach { top = top, left = left, right = right, bottom = bottom }


borderBottom : Attribute msg
borderBottom =
  Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }


-- COLORS

gray : Color
gray =
  rgb255 175 175 175


black : Color
black =
  rgb255 0 0 0



-- FONT


regularFont : Attribute msg
regularFont =
  family [ typeface "LMRoman10-Regular", sansSerif ]


mathFont : Attribute msg
mathFont =
  family [ typeface "LMRoman-Math", sansSerif ]


boldFont : Attribute msg
boldFont =
  family [ typeface "LMRoman10-Bold", sansSerif ]



-- DOM


refocus : msg -> Float -> String -> Cmd msg
refocus msg px id_ =
   -- TODO find better way of adjusting scroll
  Dom.focus id_
    |> Task.andThen (\_ -> Dom.getViewport)
    |> Task.andThen (\{viewport} -> Dom.setViewport viewport.x (viewport.y + px))
    |> Task.attempt (\_ -> msg)
