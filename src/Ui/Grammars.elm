module Ui.Grammars exposing (Model, init, validate, Msg, update, viewStatic, viewEditable)

import Html
import Html.Attributes
import Browser
import Element.Input as Input
import Element as El exposing (..)
import Element.Font as Font
import Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Data.Grammar as Grammar exposing (Grammar)
import List.Extra
import String.Extra
import Set exposing (Set)
import Task
import Ui.Input
import Ui.Utils


type alias Model =
  OneOrMore Inputs


type alias Inputs =
  { variable : String
  , syntaxes : OneOrMore String
  }



-- API


empty : Inputs
empty =
  { variable = ""
  , syntaxes = OneOrMore.init "" []
  }


isEmpty : Inputs -> Bool
isEmpty inputs =
  String.Extra.isBlank inputs.variable && (OneOrMore.all String.Extra.isBlank inputs.syntaxes)


isUnfinished : Inputs -> Bool
isUnfinished inputs =
  String.Extra.isBlank inputs.variable || (OneOrMore.any String.Extra.isBlank inputs.syntaxes)



-- INIT


init : OneOrMore Grammar -> Model
init grammars =
  let toInputs grammar =
        { variable = Grammar.variable grammar
        , syntaxes = Grammar.syntaxes grammar
        }
  in
  OneOrMore.map toInputs grammars
    |> OneOrMore.add empty



-- VALIDATE


validate : Model -> Result String (OneOrMore Grammar)
validate values =
  let toGrammar inputs =
        Grammar.init inputs.variable inputs.syntaxes
  in
  Ok (OneOrMore.map toGrammar values)



-- UPDATE


type Msg
  = OnEditVariable Int String
  | OnEditSyntaxes Int Int String
  | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnEditVariable index value ->
      let newModel =
            OneOrMore.updateAt index (\inputs -> { inputs | variable = value }) model
              |> OneOrMore.trailing isEmpty
              |> OneOrMore.add empty
      in
      ( newModel
      , Cmd.none
      )

    OnEditSyntaxes index syntaxIndex value ->
      let newModel =
            OneOrMore.updateAt index updateVariables model
              |> OneOrMore.trailing isEmpty
              |> OneOrMore.add empty

          updateVariables inputs =
            { inputs | syntaxes =
                OneOrMore.updateAt syntaxIndex (\_ -> value) inputs.syntaxes
                  |> OneOrMore.trailing String.Extra.isBlank
                  |> OneOrMore.add ""
            }
      in
      ( newModel
      , Ui.Utils.refocus NoOp (offset model newModel) (syntaxId index syntaxIndex)
        -- TODO shift not working on edit syntax
      )

    NoOp ->
      ( model, Cmd.none )


offset : Model -> Model -> Float
offset model newModel =
  toFloat (OneOrMore.length newModel - OneOrMore.length model) * 32



-- VIEW


viewStatic : OneOrMore Grammar -> Element msg
viewStatic grammars =
  let view grammar =
        indexedTable
          [ centerX, spacing 5 ]
          { data = OneOrMore.values (Grammar.syntaxes grammar)
          , columns =
              [ { header = none
                , width = px 30
                , view = \index _ ->
                    if index == 0 then
                      el
                        [ Ui.Utils.mathFont, Font.italic, Font.alignRight ]
                        (text (Grammar.variable grammar))
                    else
                      none
                }
              , { header = none
                , width = shrink
                , view = \index _ -> el [Font.alignRight] (text <| if index == 0 then " ::=" else "|")
                }
              , { header = none
                , width = shrink
                , view = \_ syntax -> el [ Ui.Utils.mathFont, Font.italic ] (text syntax)
                }
              ]
          }
  in
  column
    [ paddingXY 0 30, centerX, spacing 20 ]
    (List.map view (OneOrMore.values grammars))


viewEditable : OneOrMore Inputs -> Element Msg
viewEditable model =
  let view index inputs =
        indexedTable
          [ centerX, spacing 5 ]
          { data = OneOrMore.values inputs.syntaxes
          , columns =
              [ { header = none
                , width = px 30
                , view = viewVariable index inputs
                }
              , { header = none
                , width = shrink
                , view = viewEqualSign index inputs
                }
              , { header = none
                , width = shrink
                , view = viewSyntax index inputs
                }
              ]
          }
  in
  column
    [ paddingXY 0 30, centerX, spacing 20 ]
    (List.indexedMap view (OneOrMore.values model))


viewVariable : Int -> Inputs -> Int -> String -> Element Msg
viewVariable index inputs syntaxIndex _ =
  if syntaxIndex == 0 then
    Ui.Input.text
      [ Ui.Utils.mathFont
      , Font.italic
      ]
      { onChange = OnEditVariable index
      , text = inputs.variable
      , placeholder = "a"
      , label = "Grammar number " ++ String.fromInt index
      , minWidth = 30
      , rightAlign = True
      }
  else
    none


viewEqualSign : Int -> Inputs -> Int -> String -> Element Msg
viewEqualSign _ inputs syntaxIndex _ =
  el
    [ Font.alignRight
    , Font.color (if isEmpty inputs then Ui.Utils.gray else Ui.Utils.black)
    ]
    (text (if syntaxIndex == 0 then " ::=" else "|"))


viewSyntax : Int -> Inputs -> Int -> String -> Element Msg
viewSyntax index inputs syntaxIndex syntax =
  Ui.Input.text
    [ Ui.Utils.mathFont
    , Font.italic
    , Ui.Utils.id (syntaxId index syntaxIndex)
    ]
    { onChange = OnEditSyntaxes index syntaxIndex
    , text = syntax
    , placeholder = "a + b"
    , label = "Syntax number " ++ String.fromInt syntaxIndex
    , minWidth = 100
    , rightAlign = False
    }


syntaxId : Int -> Int -> String
syntaxId index syntaxIndex =
  "grammar-syntaxes-" ++ String.fromInt index ++ "-" ++ String.fromInt syntaxIndex
