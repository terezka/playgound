module Main exposing (..)

import Browser
import Element.Border as Border
import Element.Input as Input
import Element as El exposing (..)
import Element.Font as Font exposing (..)
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import List.Extra


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { template : Maybe Language
  , editing : Maybe Int
  , domains : OneOrMore Domain
  , grammar : OneOrMore Grammar
  , semantics : OneOrMore Semantics
  , expression : String
  , result : Result (List Error) (List State)
  }


type Domain
  = Domain Int String (List String)


type Semantics
  = Axiom Step
  | Rule (List Step) Step


type Step
  = Step Expression Expression


type alias Variable
  = String


type Grammar
  = Grammar String (List String)


type alias Expression
  = String

type alias Language
  = String

type alias State
  = String

type alias Error
  = String




-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
  ( { template = Nothing
    , editing = Just 1
    , domains =
        OneOrMore
          (Domain 1 "Variable" ["x", "y", "z"])
          [ Domain 2 "Integer" ["n", "m"]
          , Domain 3 "Expression" ["e", "e₀", "e₁"]
          , Domain 4 "" []
          ]
    , grammar =
        OneOrMore
          (Grammar "e" [ "x", "n", "e1 + e2", "e1 × e2", "x := e1;e2", "" ])
          []
    , semantics =
        OneOrMore
          (Axiom (Step "(λx. e) v" "e{v/x}"))
          [ Rule [Step "e" "e′"] (Step "E[e]" "E[e′]")
          ]
    , expression = "(\\x.x)"
    , result = Err []
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = OnDomainEditVars Int String
  | OnDomainEditName Int String
  | OnGrammarEditSyntax Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnDomainEditVars searched vars ->
      let updateOne (Domain id name existing) =
            if searched == id then
              Domain id name (String.split " , " vars)
            else
              Domain id name existing
      in
      ( { model | domains = OneOrMore.map updateOne model.domains }
      , Cmd.none
      )

    OnDomainEditName searched name ->
      let updateOne (Domain id existing vars) =
            if searched == id then
              Domain id name vars
            else
              Domain id existing vars
      in
      ( { model | domains = OneOrMore.map updateOne model.domains }
      , Cmd.none
      )

    OnGrammarEditSyntax searchedGrammerId searchedSyntaxId syntax ->
      let updateOne index (Grammar name existing) =
            if searchedGrammerId == index then
              List.indexedMap (\i s -> if i == searchedSyntaxId then syntax else s) existing
                |> Grammar name
            else
              Grammar name existing
      in
      ( { model | grammar = OneOrMore.indexedMap updateOne model.grammar }
      , Cmd.none
      )


view : Model -> Browser.Document Msg
view model =
  { title = "Playground"
  , body =
      [ layout [ width fill ] <|
          column
            [ width (px 900)
            , centerX
            , family [ typeface "LMRoman10-Regular", sansSerif ]
            ]
            [ title "Programming Language Playground"
            , paragraph [] [ text "For each of the following simply-typed lambda calculus expressions (including products, sums, and references), state whether the expression is well-typed or not. If it is well-typed, then give the type of the expression." ]
            --, el [ centerX ] [ select OnTemplate languages ]
            , stepTitle 1 "Define the grammar" model.editing
            , paragraph [] [ text "We saw in class the lambda calculus extended with references. In this question, you will give a CPS translation from the lambda calculus with references to the lambda calculus with products, integers, and booleans." ]
            , viewDomains model.domains model.editing
            , viewGrammar model.grammar model.editing
            -- , viewErrors onlyGrammar model.result
            , stepTitle 2 "Define the semantics" model.editing
            , viewSemantics model.semantics
            -- , viewErrors onlySemantics model.result
            , stepTitle 3 "Evaluate an expression" model.editing
            -- , editor model.expression
            -- , viewSteps model.result
            ]
      ]
  }


viewDomains : OneOrMore Domain -> Maybe Int -> Element Msg
viewDomains (OneOrMore first rest) editing =
  let viewLeftSide (Domain id name vars) =
        case editing of
          Just 1 ->
            Input.text
              [ mathFont
              , italic
              , borderBottom
              , Border.dashed
              , Border.color gray
              , paddingXY 3 3
              , Font.alignRight
              ]
              { onChange = OnDomainEditVars id
              , text = String.join ", " vars
              , placeholder = Just (placeholder [El.alignRight] "a, b, c")
              , label = Input.labelHidden ("Variables of domain number " ++ String.fromInt id)
              }

          _ ->
            el [ mathFont, italic, Font.alignRight ] (text (String.join ", " vars))

      viewMiddle (Domain id name vars) =
        if String.isEmpty name && List.isEmpty vars then
            el [ mathFont, Font.color gray ] (text " ∈ ")
        else
            el [ mathFont ] (text " ∈ ")

      viewRightSide (Domain id name vars) =
        case editing of
          Just 1 ->
            Input.text
              [ borderBottom
              , Border.dashed
              , Border.color gray
              , paddingXY 3 3
              ]
              { onChange = OnDomainEditName id
              , text = name
              , placeholder = Just (placeholder [] "Another")
              , label = Input.labelHidden ("Name of domain number " ++ String.fromInt id)
              }

          _ ->
            el [ boldFont, bold ] (text name)
  in
  el [ centerX, paddingXY 0 20 ] <|
    table
      [ centerX, spacing 5 ]
      { data = first :: rest
      , columns =
          [ { header = text ""
            , width = shrink
            , view = viewLeftSide
            }
          , { header = text ""
            , width = shrink
            , view = viewMiddle
            }
          , { header = text ""
            , width = shrink
            , view = viewRightSide
            }
          ]
      }


viewGrammar : OneOrMore Grammar -> Maybe Int -> Element Msg
viewGrammar (OneOrMore first rest) editing =
  let viewSingle indexGrammer (Grammar var syntaxs) =
        column [ spacing 3 ] <|
          case syntaxs of
            one :: remaining ->
              [ row [] [ viewStart var, viewSyntax indexGrammer 0 one ]
              ]
              ++ (List.indexedMap (\i s -> withDeliniator (viewSyntax indexGrammer (i + 1) s)) remaining)

            [] ->
              []

      viewStart var =
        row [] [ el [ mathFont, italic ] (text var), el [] (text " ::= ") ]

      withDeliniator show  =
        row [] [ el [] (text "     | "), show ]

      viewSyntax indexGrammer indexSyntax syntax =
        case editing of
          Just 1 ->
            Input.text
              [ borderBottom
              , Border.dashed
              , Border.color gray
              , paddingXY 3 3
              , mathFont
              , italic
              ]
              { onChange = OnGrammarEditSyntax indexGrammer indexSyntax
              , text = syntax
              , placeholder = Just (placeholder [] "a + b")
              , label = Input.labelHidden ("Syntax number " ++ String.fromInt indexSyntax)
              }

          _ -> el [ mathFont, italic ] (text syntax)
  in
  column
    [ paddingXY 0 30, centerX, spacing 20 ]
    (List.indexedMap viewSingle (first :: rest))


viewSemantics : OneOrMore Semantics -> Element Msg
viewSemantics (OneOrMore first rest) =
  let viewSingle semantics =
        case semantics of
          Axiom conclusion -> viewRule [] conclusion
          Rule precs conclusion -> viewRule precs conclusion

      viewRule precs conclusion =
        column
          []
          [ row [ height (px 25), centerX ] (List.map viewStep precs)
          , el
              [ width (fill |> minimum 200)
              , borderBottom
              ]
              (text "")
          , el [ centerX, paddingXY 0 5 ] (viewStep conclusion)
          ]

      viewStep (Step a b) =
        el [ mathFont, italic ] (text (a ++ " → " ++ b))
  in
  row
    [ width fill, paddingXY 40 20, centerX, spacing 40 ]
    (List.map viewSingle (first :: rest))



-- VIEW ELEMENTS


title : String -> Element Msg
title string =
  el [ paddingXY 0 40, centerX, size 35 ] (text string)


stepTitle : Int -> String -> Maybe Int -> Element Msg
stepTitle number string editing =
  row [ width fill ]
    [ el [ paddingXY 0 30, El.alignLeft, size 25 ] (text <| String.fromInt number ++ ". " ++ string)
    , el [ El.alignRight, underline, size 16 ] (text (if editing == Just number then "Save" else "Edit"))
    ]


gray : Color
gray =
  rgb255 175 175 175


borderBottom : Attribute Msg
borderBottom =
  Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }


placeholder : List (Attribute Msg) -> String -> Input.Placeholder Msg
placeholder attrs string =
  Input.placeholder [] (el (Font.color gray :: attrs) (text string))


-- FONT

mathFont : Attribute Msg
mathFont =
  family [ typeface "LMRoman-Math", sansSerif ]


boldFont : Attribute Msg
boldFont =
  family [ typeface "LMRoman10-Bold", sansSerif ]
