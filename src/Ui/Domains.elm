module Ui.Domains exposing (Model, init, validate, Msg, update, viewEditable, viewStatic)

import Html
import Html.Attributes
import Browser
import Element.Input as Input
import Element as El exposing (..)
import Element.Font as Font
import Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Data.Domain as Domain exposing (Domain)
import List.Extra
import String.Extra
import Set exposing (Set)
import Task
import Ui.Input
import Ui.Utils


-- MODEL


type alias Model =
  OneOrMore Inputs


type alias Inputs =
  { name : String
  , variables : String
  }



-- API


empty : Inputs
empty =
  { name = ""
  , variables = ""
  }


isEmpty : Inputs -> Bool
isEmpty inputs =
  String.Extra.isBlank inputs.name && String.Extra.isBlank inputs.variables


isUnfinished : Inputs -> Bool
isUnfinished inputs =
  String.Extra.isBlank inputs.name || String.Extra.isBlank inputs.variables



-- INIT


init : OneOrMore Domain -> Model
init domains =
  let toInputs domain =
        { name = Domain.name domain
        , variables =
            Domain.variables domain
              |> Set.toList
              |> String.join ", "
        }
  in
  OneOrMore.map toInputs domains
    |> OneOrMore.add empty



-- VALIDATE


validate : Model -> Result String (OneOrMore Domain)
validate values =
  let toDomains inputs =
        OneOrMore.map toDomain inputs

      toDomain (name, variables) =
        Domain.init
          (String.Extra.clean name)
          (List.sort variables)
  in
  Ok values
    |> Result.andThen isOneFilledOut
    |> Result.andThen isFilledOutCorrectly
    |> Result.andThen hasUniqueNames
    |> Result.andThen hasUniqueVariables
    |> Result.map toDomains


isOneFilledOut : Model -> Result String Model
isOneFilledOut values =
  OneOrMore.filter (not << isEmpty) values
    |> Maybe.map Ok
    |> Maybe.withDefault (Err "You must have at least one domain!")


isFilledOutCorrectly : Model -> Result String Model
isFilledOutCorrectly values =
  if OneOrMore.any isUnfinished values then
    Err "Domains must have a variable list and a name. If you'd like to discard an entry, delete both fields."
  else
    Ok values


hasUniqueNames : Model -> Result String Model
hasUniqueNames values =
  let names =
        OneOrMore.values values
          |> List.map .name
  in
  if onlyUnique names then
    Ok values
  else
    Err "Your domains must all have unique names!"


hasUniqueVariables : Model -> Result String (OneOrMore (String, List String))
hasUniqueVariables values =
  let processedValues =
        OneOrMore.map splitVariables values

      splitVariables inputs =
        ( inputs.name
        , inputs.variables
            |> String.split ","
            |> List.map String.Extra.clean
        )

      allUnique =
        OneOrMore.values processedValues
          |> List.concatMap Tuple.second
          |> onlyUnique
  in
  if allUnique then
    Ok processedValues
  else
    Err "Your domains must all have unique variables!"


onlyUnique : List comparable -> Bool
onlyUnique values =
  Set.size (Set.fromList values) == List.length values



-- UPDATE


type Msg
  = OnEditName Int String
  | OnEditVariables Int String
  | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnEditName index value ->
      let newModel =
            OneOrMore.updateAt index (\inputs -> { inputs | name = value }) model
              |> OneOrMore.trailing isEmpty
              |> OneOrMore.add empty
      in
      ( newModel
      , Ui.Utils.refocus NoOp (offset model newModel) (nameId index)
      )

    OnEditVariables index value ->
      let newModel =
            OneOrMore.updateAt index (\inputs -> { inputs | variables = value }) model
              |> OneOrMore.trailing isEmpty
              |> OneOrMore.add empty
      in
      ( newModel
      , Ui.Utils.refocus NoOp (offset model newModel) (variablesId index)
      )

    NoOp ->
      ( model, Cmd.none )


offset : Model -> Model -> Float
offset model newModel =
  toFloat (OneOrMore.length newModel - OneOrMore.length model) * 32


-- VIEW STATIC


viewStatic : OneOrMore Domain -> Element msg
viewStatic model =
  el [ centerX, paddingXY 0 20 ] <|
    table
      [ centerX, spacing 5 ]
      { data = OneOrMore.values model
      , columns =
          [ { header = none
            , width = shrink
            , view = \domain ->
                let variables =
                      Domain.variables domain
                        |> Set.toList
                        |> String.join ", "
                in
                el [ Font.italic, Ui.Utils.mathFont ] (text variables)
            }
          , { header = none
            , width = shrink
            , view = \domain -> el [ Ui.Utils.mathFont ] (text " ∈ ")
            }
          , { header = none
            , width = shrink
            , view = \domain -> el [ Font.italic, Ui.Utils.mathFont ] (text (Domain.name domain))
            }
          ]
      }


-- VIEW


viewEditable : Model -> Element Msg
viewEditable model =
  el [ centerX, paddingXY 0 20 ] <|
    indexedTable
      [ centerX, spacing 5 ]
      { data = OneOrMore.values model
      , columns =
          [ { header = none
            , width = shrink
            , view = viewVariables
            }
          , { header = none
            , width = shrink
            , view = viewBelongSymbol
            }
          , { header = none
            , width = shrink
            , view = viewName
            }
          ]
      }


viewVariables : Int -> Inputs -> Element Msg
viewVariables index inputs =
  Ui.Input.text
    [ Font.italic
    , Ui.Utils.mathFont
    , Ui.Utils.id (variablesId index)
    ]
    { onChange = OnEditVariables index
    , text = inputs.variables
    , placeholder = "a, b, c"
    , label = "Variables of domain number " ++ String.fromInt index
    , minWidth = 70
    , rightAlign = True
    }


viewBelongSymbol : Int -> Inputs -> Element Msg
viewBelongSymbol index inputs =
  el [ Ui.Utils.mathFont, Font.color (if isEmpty inputs then Ui.Utils.gray else Ui.Utils.black) ] (text " ∈ ")


viewName : Int -> Inputs -> Element Msg
viewName index inputs =
  Ui.Input.text
    [ Ui.Utils.id (nameId index) ]
    { onChange = OnEditName index
    , text = inputs.name
    , placeholder = "Another"
    , label = "Name of domain number " ++ String.fromInt index
    , minWidth = 100
    , rightAlign = False
    }



-- IDS


variablesId : Int -> String
variablesId index =
  "domain-variables-index-" ++ String.fromInt index


nameId : Int -> String
nameId index =
  "domain-name-index-" ++ String.fromInt index

