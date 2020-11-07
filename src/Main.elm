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
import Data.Language as Language exposing (Language)
import Data.Domain as Domain exposing (Domain(..))
import Data.Grammar as Grammar exposing (Grammar(..))
import Data.Rule as Rule exposing (Rule(..), Step(..))
import Language.SimpleLambdaCalculus
import Ui.Input
import Ui.Domains


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { validated : Language
  , domains : Maybe Ui.Domains.Model
  , grammars : Maybe (OneOrMore Grammar)
  , semantics : Maybe (OneOrMore Rule)
  , expression : String
  , result : Result (List String) ()
  }


type Step
  = DefineDomains
  | DefineGrammar
  | DefineSemantics



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
  ( { validated = Language.SimpleLambdaCalculus.config
    , domains = Nothing
    , grammars = Nothing
    , semantics = Nothing
    , expression = Language.SimpleLambdaCalculus.config.expression
    , result = Err []
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = NoOp
  | OnToggleEdit Int
  | DomainMsg Ui.Domains.Msg
  | OnGrammarEditName Int String
  | OnGrammarEditSyntax Int Int String
  | OnRuleRight Int Int String
  | OnRuleLeft Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({validated} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    OnToggleEdit step ->
      case step of
        1 ->
          case model.domains of
            Nothing ->
              ( { model | domains = Just (Ui.Domains.init validated.domains) }
              , Cmd.none
              )

            Just domains ->
              case Ui.Domains.validate domains of
                Ok valid ->
                  ( { model
                    | validated = { validated | domains = valid }
                    , domains = Nothing
                    , result = Ok ()
                    }
                  , Cmd.none
                  )

                Err error ->
                  ( { model | result = Err [error] }
                  , Cmd.none
                  )

        2 ->
          case model.grammars of
            Nothing ->
              ( { model
                | grammars =
                    validated.grammars
                      |> OneOrMore.map Grammar.withEmpty
                      |> OneOrMore.add Grammar.empty
                      |> Just
                }
              , Cmd.none
              )

            Just grammars ->
              case OneOrMore.filterMap Grammar.clean grammars of
                Just valid ->
                  ( { model | grammars = Just valid }
                  , Cmd.none
                  )

                Nothing ->
                  ( { model | result = Err [ "You must have at least one grammar." ] }
                  , Cmd.none
                  )

        _ -> -- TODO
          (model, Cmd.none)

    DomainMsg subMsg ->
      case model.domains of
        Just domains ->
          Ui.Domains.update subMsg domains
            |> Tuple.mapFirst (\subModel -> { model | domains = Just subModel })
            |> Tuple.mapSecond (Cmd.map DomainMsg)

        Nothing ->
          ( model, Cmd.none )

    OnGrammarEditName index name ->
      case model.grammars of
        Just grammars ->
          let update_ (Grammar _ syntaxes) =
                Grammar name syntaxes
          in
          ( { model | grammars = Just (OneOrMore.updateAt index update_ grammars) }
          , Cmd.none
          )

        Nothing ->
          ( model, Cmd.none )

    OnGrammarEditSyntax index syntaxIndex newSyntax ->
      case model.grammars of
        Just grammars ->
          let isLast =
                OneOrMore.isLast index grammars

              isLastSyntax =
                OneOrMore.getAt index grammars
                  |> Maybe.map (Grammar.syntaxes >> OneOrMore.isLast syntaxIndex)
                  |> Maybe.withDefault False

              update_ (Grammar name syntaxes) =
                OneOrMore.updateAt syntaxIndex (always newSyntax) syntaxes
                  |> (if isLastSyntax then OneOrMore.add "" else identity)
                  |> Grammar name
          in
          ( { model
            | grammars =
                OneOrMore.updateAt index update_ grammars
                  |> (if isLast then OneOrMore.add Grammar.empty else identity)
                  |> Just
            }
          , if isLastSyntax then
              refocus -32 (grammarSyntaxId index syntaxIndex)
            else
              Cmd.none
          )

        Nothing ->
          ( model, Cmd.none )

    OnRuleLeft index ruleIndex new ->
      case model.semantics of
        Just semantics ->
          let isLast =
                OneOrMore.isLast index semantics

              isLastCondition =
                OneOrMore.getAt index semantics
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
                OneOrMore.updateAt index update_ semantics
                  |> (if isLast then OneOrMore.add (Rule.withEmpty Rule.empty) else identity)
                  |> Just
            }
          , Cmd.none
          )

        Nothing ->
          ( model, Cmd.none )

    OnRuleRight index ruleIndex new ->
      case model.semantics of
        Just semantics ->
          let update_ (Rule precs conclusion) =
                if ruleIndex == -1 then
                  Rule precs (updateStep conclusion)
                else
                  Rule (List.Extra.updateAt ruleIndex updateStep precs) conclusion

              updateStep (Step a b) =
                Step a new
          in
          ( { model | semantics = Just (OneOrMore.updateAt index update_ semantics) }
          , Cmd.none
          )

        Nothing ->
          ( model, Cmd.none )



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
            , paragraph [] [ text "We saw in class the lambda calculus extended with references. In this question, you will give a CPS translation from the lambda calculus with references to the lambda calculus with products, integers, and booleans." ]
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
            , stepTitle 1 "Define the domains" model.domains
            , paragraph []
                [ text
                  """Here we specify our domains and our grammar. There are a few special domains.
                  """
                ]
            , case model.domains of
                Just domains ->
                  map DomainMsg (Ui.Domains.view domains)

                Nothing ->
                  Ui.Domains.viewStatic model.validated.domains
            --, stepTitle 1 "Define the grammar" model.editing
            --, viewGrammar model.grammars model.editing
            --, stepTitle 2 "Define the semantics" model.editing
            --, viewSemantics model.semantics model.editing
            , el [ paddingXY 0 30, El.alignLeft, size 25 ] (text "3. Evaluate an expression")
            , viewEvaluator model
            ]
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
                , placeholder = "e"
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
                , placeholder = "e′"
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


viewEvaluator : Model -> Element Msg
viewEvaluator model =
  el [width fill, paddingTRBL 0 0 70 0] <|
    Input.multiline
      [ Border.width 1
      , Border.color black
      , Border.rounded 0
      , paddingTRBL 0 0 40 0
      , width fill
      , mathFont
      ]
      { onChange = \_ -> NoOp
      , text = model.expression
      , placeholder =
          Just <| Input.placeholder [] (el [ Font.color gray ] (El.text "λx. x"))
      , label = Input.labelHidden "Expression"
      , spellcheck = False
      }


-- ELEMENTS


title : String -> Element Msg
title string =
  el [ paddingXY 0 40, centerX, size 35 ] (text string)


stepTitle : Int -> String -> Maybe a -> Element Msg
stepTitle number string state =
  row
    [ width fill ]
    [ el [ paddingXY 0 30, El.alignLeft, size 25 ] (text <| String.fromInt number ++ ". " ++ string)
    , el [ El.alignRight, underline, size 16 ] <|
        Input.button
          []
          { onPress = Just (OnToggleEdit number)
          , label = text <|
              case state of
                Just _ -> "Save"
                Nothing -> "Edit"
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
