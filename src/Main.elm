module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Element.Border as Border
import Element.Input as Input
import Element as El exposing (..)
import Element.Font as Font exposing (..)
import Data.OneOrMore as OneOrMore exposing (OneOrMore(..))
import List.Extra
import Html
import Html.Attributes
import Set exposing (Set)
import Task
import Data.Domain as Domain exposing (Domain(..))
import Data.Grammar as Grammar exposing (Grammar(..))
import Data.Rule as Rule exposing (Rule(..), Step(..))
import Ui.Input


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { template : Maybe Language
  , editing : Set Int
  , domains : OneOrMore Domain
  , grammars : OneOrMore Grammar
  , semantics : OneOrMore Rule
  , expression : String
  , result : Result (List Error) (List State)
  }


type alias Variable
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
    , editing = Set.empty
    , domains =
        OneOrMore
          (Domain "Variable" ["x", "y", "z"])
          [ Domain "Integer" ["n", "m"]
          , Domain "Expression" ["e", "e₀", "e₁"]
          ]
    , grammars =
        OneOrMore
          (Grammar.init "e" "x" [ "n", "e1 + e2", "e1 × e2", "x := e1;e2" ])
          []
    , semantics =
        OneOrMore
          (Rule [] (Step "(λx. e) v" "e{v/x}"))
          [ Rule [Step "e" "e′"] (Step "E[e]" "E[e′]")
          ]
    , expression = "(\\x.x)"
    , result = Err []
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = NoOp
  | OnToggleEdit Int
  | OnDomainEditVars Int String
  | OnDomainEditName Int String
  | OnGrammarEditName Int String
  | OnGrammarEditSyntax Int Int String
  | OnRuleRight Int Int String
  | OnRuleLeft Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    OnToggleEdit step ->
      ( if Set.member step model.editing
            then
              case OneOrMore.filter (not << Domain.isEmpty) model.domains of
                Just domains ->
                  case OneOrMore.filterMap Grammar.clean model.grammars of
                    Just grammars ->
                      { model
                      | editing = Set.remove step model.editing
                      , domains = domains
                      , grammars = grammars
                      }

                    Nothing ->
                      { model | result = Err [ "You must have at least one grammar." ] }

                Nothing ->
                  { model | result = Err [ "You must have at least one set." ] }
            else
              { model
              | editing = Set.insert step model.editing
              , domains =
                  if step == 1 then
                    model.domains
                      |> OneOrMore.add Domain.empty
                  else
                    model.domains
              , grammars =
                  if step == 1 then
                    model.grammars
                      |> OneOrMore.map Grammar.withEmpty
                      |> OneOrMore.add Grammar.empty
                  else
                    model.grammars
              , semantics =
                  if step == 2 then
                    model.semantics
                      |> OneOrMore.add Rule.empty
                      |> OneOrMore.map Rule.withEmpty
                  else
                    model.semantics
              }

      , Cmd.none
      )

    OnDomainEditVars index vars ->
      let isLast =
            OneOrMore.isLast index model.domains

          update_ (Domain name _) =
            Domain name (String.split ", " vars)
      in
      ( { model
        | domains =
            OneOrMore.updateAt index update_ model.domains
              |> (if isLast then OneOrMore.add Domain.empty else identity)
        }
      , if isLast then
          refocus -32 (domainVarsId index)
        else
          Cmd.none
      )

    OnDomainEditName index name ->
      let update_ (Domain _ vars) =
            Domain name vars
      in
      ( { model | domains = OneOrMore.updateAt index update_ model.domains }
      , Cmd.none
      )

    OnGrammarEditName index name ->
      let update_ (Grammar _ syntaxes) =
            Grammar name syntaxes
      in
      ( { model | grammars = OneOrMore.updateAt index update_ model.grammars }
      , Cmd.none
      )

    OnGrammarEditSyntax index syntaxIndex newSyntax ->
      let isLast =
            OneOrMore.isLast index model.grammars

          isLastSyntax =
            OneOrMore.getAt index model.grammars
              |> Maybe.map (Grammar.syntaxes >> OneOrMore.isLast syntaxIndex)
              |> Maybe.withDefault False

          update_ (Grammar name syntaxes) =
            OneOrMore.updateAt syntaxIndex (always newSyntax) syntaxes
              |> (if isLastSyntax then OneOrMore.add "" else identity)
              |> Grammar name
      in
      ( { model
        | grammars =
            OneOrMore.updateAt index update_ model.grammars
              |> (if isLast then OneOrMore.add Grammar.empty else identity)
        }
      , if isLastSyntax then
          refocus -32 (grammarSyntaxId index syntaxIndex)
        else
          Cmd.none
      )

    OnRuleLeft index ruleIndex new ->
      let isLast =
            OneOrMore.isLast index model.semantics

          isLastCondition =
            OneOrMore.getAt index model.semantics
              |> Maybe.map ((==) (ruleIndex + 1) << List.length << Rule.conditions)
              |> Maybe.withDefault False

          update_ (Rule precs conclusion) =
            if ruleIndex == -1 then
              Rule precs (updateStep conclusion)
            else
              Rule (List.Extra.updateAt ruleIndex updateStep precs) conclusion
                |> (if isLastCondition then Rule.withEmpty else identity)

          updateStep (Step a b) =
            Step new b
      in
      ( { model
        | semantics =
            OneOrMore.updateAt index update_ model.semantics
              |> (if isLast then OneOrMore.add Rule.empty else identity)
        }
      , Cmd.none
      )

    OnRuleRight index ruleIndex new ->
      let update_ (Rule precs conclusion) =
            if ruleIndex == -1 then
              Rule precs (updateStep conclusion)
            else
              Rule (List.Extra.updateAt ruleIndex updateStep precs) conclusion

          updateStep (Step a b) =
            Step a new
      in
      ( { model | semantics = OneOrMore.updateAt index update_ model.semantics }
      , Cmd.none
      )



refocus : Float -> String -> Cmd Msg
refocus px id =
  Dom.focus id
    |> Task.andThen (\_ -> Dom.getViewport)
    |> Task.andThen (\{viewport} -> Dom.setViewport viewport.x (viewport.y + px)) -- TODO
    |> Task.attempt (\_ -> NoOp)



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Playground"
  , body =
      [ layout [ width fill ] <|
          column
            [ width (px 900)
            , centerX
            , regularFont
            ]
            [ title "Programming Language Playground"
            , paragraph [] [ text "For each of the following simply-typed lambda calculus expressions (including products, sums, and references), state whether the expression is well-typed or not. If it is well-typed, then give the type of the expression." ]
            , case model.result of -- TODO
                Err [] -> none

                Err errors ->
                  el [paddingTRBL 20 0 0 0, width fill] <|
                   row
                    [ Border.color black
                    , Border.width 1
                    , paddingXY 20 10
                    , width fill
                    ]
                    (List.map text errors)

                Ok _ -> none
            , stepTitle 1 "Define the grammar" model.editing
            , paragraph [] [ text "We saw in class the lambda calculus extended with references. In this question, you will give a CPS translation from the lambda calculus with references to the lambda calculus with products, integers, and booleans." ]
            , viewDomains model.domains model.editing
            , viewGrammar model.grammars model.editing
            , stepTitle 2 "Define the semantics" model.editing
            , viewSemantics model.semantics model.editing
            , stepTitle 3 "Evaluate an expression" model.editing
            ]
      ]
  }


viewDomains : OneOrMore Domain -> Set Int -> Element Msg
viewDomains (OneOrMore first rest) editing =
  let viewLeftSide index (Domain name vars) =
        if Set.member 1 editing then
            Ui.Input.text
              [ mathFont
              , italic
              , htmlAttribute (Html.Attributes.id (domainVarsId index))
              ]
              { onChange = OnDomainEditVars index
              , text = String.join ", " vars
              , placeholder = "a, b, c"
              , label = "Variables of domain number " ++ String.fromInt index
              , minWidth = 70
              , rightAlign = True
              }
        else
          el [ mathFont, italic, Font.alignRight ] (text (String.join ", " vars))

      viewMiddle _ (Domain name vars) =
        if String.isEmpty name && List.isEmpty vars then
            el [ mathFont, Font.color gray ] (text " ∈ ")
        else
            el [ mathFont ] (text " ∈ ")

      viewRightSide index (Domain name vars) =
        if Set.member 1 editing then
          Ui.Input.text
            []
            { onChange = OnDomainEditName index
            , text = name
            , placeholder = "Another"
            , label = "Name of domain number " ++ String.fromInt index
            , minWidth = 100
            , rightAlign = False
            }
        else
          el [ boldFont, bold ] (text name)
  in
  el [ centerX, paddingXY 0 20 ] <|
    indexedTable
      [ centerX, spacing 5 ]
      { data = first :: rest
      , columns =
          [ { header = none
            , width = shrink
            , view = viewLeftSide
            }
          , { header = none
            , width = shrink
            , view = viewMiddle
            }
          , { header = none
            , width = shrink
            , view = viewRightSide
            }
          ]
      }


viewGrammar : OneOrMore Grammar -> Set Int -> Element Msg
viewGrammar (OneOrMore first rest) editing =
  let viewOne index grammar =
        indexedTable
          [ centerX, spacing 5 ]
          { data = OneOrMore.all (Grammar.syntaxes grammar)
          , columns =
              [ { header = none
                , width = px 30
                , view = viewLeftSide grammar index
                }
              , { header = none
                , width = shrink
                , view = viewMiddle grammar index
                }
              , { header = none
                , width = shrink
                , view = viewRightSide grammar index
                }
              ]
          }

      viewLeftSide grammar index syntaxIndex _ =
        if syntaxIndex /= 0 then
          none
        else if Set.member 1 editing then
          Ui.Input.text
            [ mathFont
            , italic
            ]
            { onChange = OnGrammarEditName index
            , text = Grammar.variable grammar
            , placeholder = "a"
            , label = "Grammar number " ++ String.fromInt index
            , minWidth = 30
            , rightAlign = True
            }
        else
          el [ mathFont, italic, Font.alignRight ] (text (Grammar.variable grammar))

      viewMiddle grammar _ syntaxIndex _ =
        el
          (if Grammar.isEmpty grammar then [ Font.alignRight, Font.color gray ] else [Font.alignRight])
          (text (if syntaxIndex == 0 then " ::=" else "|")) -- TODO

      viewRightSide grammar index syntaxIndex syntax =
        if Set.member 1 editing then
          Ui.Input.text
            [ mathFont
            , italic
            , htmlAttribute (Html.Attributes.id (grammarSyntaxId index syntaxIndex))
            ]
            { onChange = OnGrammarEditSyntax index syntaxIndex
            , text = syntax
            , placeholder = "a + b"
            , label = "Syntax number " ++ String.fromInt syntaxIndex
            , minWidth = 100
            , rightAlign = False
            }
        else
          el [ mathFont, italic ] (text syntax)
  in
  column
    [ paddingXY 0 30, centerX, spacing 20 ]
    (List.indexedMap viewOne (first :: rest))


viewSemantics : OneOrMore Rule -> Set Int -> Element Msg
viewSemantics (OneOrMore first rest) editing =
  let viewRule index (Rule precs conclusion) =
        column
          []
          [ row [ height (px 25), centerX, paddingTRBL 0 0 5 0, spacing 30 ] (List.indexedMap (viewStep index) precs)
          , el
              [ width (fill |> minimum 200)
              , borderBottom
              ]
              none
          , el [ centerX, paddingXY 0 5 ] (viewStep index -1 conclusion)
          ]

      viewStep index stepIndex (Step a b) =
        if Set.member 2 editing then
          row []
            [ Ui.Input.text
                [ mathFont
                , italic
                ]
                { onChange = OnRuleLeft index stepIndex
                , text = a
                , placeholder = "a"
                , label = "Step number 1" -- TODO
                , minWidth = 50
                , rightAlign = True
                }
            , el [ mathFont, italic ] (text " → ")
            , Ui.Input.text
                [ mathFont
                , italic
                ]
                { onChange = OnRuleRight index stepIndex
                , text = b
                , placeholder = "a'"
                , label = "Step number 2" -- TODO
                , minWidth = 50
                , rightAlign = False
                }
            ]
        else
          el [ mathFont, italic ] (text (a ++ " → " ++ b))
  in
  wrappedRow
    [ width fill, paddingXY 40 20, centerX, spacing 40 ]
    (List.indexedMap viewRule (first :: rest))



-- VIEW ELEMENTS


title : String -> Element Msg
title string =
  el [ paddingXY 0 40, centerX, size 35 ] (text string)


stepTitle : Int -> String -> Set Int -> Element Msg
stepTitle number string editing =
  row [ width fill ]
    [ el [ paddingXY 0 30, El.alignLeft, size 25 ] (text <| String.fromInt number ++ ". " ++ string)
    , el [ El.alignRight, underline, size 16 ] <|
        Input.button
          []
          { onPress = Just (OnToggleEdit number)
          , label = text (if Set.member number editing then "Save" else "Edit")
          }
    ]


gray : Color
gray =
  rgb255 175 175 175


black : Color
black =
  rgb255 0 0 0


paddingTRBL : Int -> Int -> Int -> Int -> Attribute Msg
paddingTRBL top right bottom left =
  paddingEach { top = top, left = left, right = right, bottom = bottom }


borderBottom : Attribute Msg
borderBottom =
  Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }



-- FONT


regularFont : Attribute Msg
regularFont =
  family [ typeface "LMRoman10-Regular", sansSerif ]


mathFont : Attribute Msg
mathFont =
  family [ typeface "LMRoman-Math", sansSerif ]


boldFont : Attribute Msg
boldFont =
  family [ typeface "LMRoman10-Bold", sansSerif ]


-- IDS


domainVarsId : Int -> String
domainVarsId index =
  "domain-vars-" ++ String.fromInt index


grammarSyntaxId : Int -> Int -> String
grammarSyntaxId index syntaxIndex =
  "grammar-syntaxes-" ++ String.fromInt index ++ "-" ++ String.fromInt syntaxIndex
