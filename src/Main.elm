module Main exposing (..)

import Browser
import Element as El exposing (..)
import Element.Font exposing (..)

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { template : Maybe Language
  , domains : OneOrMore Domain
  , grammar : OneOrMore Grammar
  , semantics : OneOrMore Semantics
  , expression : String
  , result : Result (List Error) (List State)
  }


type Domain
  = Domain String (List Char)


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

type OneOrMore a
  = OneOrMore a (List a)


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
  ( { template = Nothing
    , domains =
        OneOrMore
          (Domain "Variable" ['x', 'y', 'z'])
          [ Domain "Integer" ['n', 'm']
          , Domain "Expression" ['e']
          ]
    , grammar =
        OneOrMore (Grammar "e" [ "x", "n", "e1 + e2", "e1 × e2", "x := e1;e2" ]) []
    , semantics = OneOrMore (Axiom (Step "x" "x'")) []
    , expression = "(\\x.x)"
    , result = Err []
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = OnTemplate

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
  { title = "Playground"
  , body =
      [ layout [ width fill ] <|
          column
            [ width (px 900)
            , centerX
            , family [ typeface "cmu_serifroman", sansSerif ]
            ]
            [ title "Programming Language Playground"
            , paragraph [] [ text "We saw in class the lambda calculus extended with references. In this question, you will give a CPS translation from the lambda calculus with references to the lambda calculus with products, integers, and booleans." ]
            --, el [ centerX ] [ select OnTemplate languages ]
            , stepTitle "1. Define the grammar"
            , viewDomains model.domains
            , viewGrammar model.grammar
            -- , viewErrors onlyGrammar model.result
            , stepTitle "2. Define the semantics"
            --, el [ centerX ] (viewSemantics model.semantics)
            -- , viewErrors onlySemantics model.result
            , stepTitle "3. Evaluate an expression"
            -- , editor model.expression
            -- , viewSteps model.result
            ]
      ]
  }


viewDomains : OneOrMore Domain -> Element Msg
viewDomains (OneOrMore first rest) =
  let viewSingle (Domain name chars) =
        el [] (text (withCommas chars ++ " ∈ " ++ name))

      withCommas chars =
        String.fromList (List.intersperse ',' chars)
  in
  column [ centerX ] (List.map viewSingle (first :: rest))


viewGrammar : OneOrMore Grammar -> Element Msg
viewGrammar (OneOrMore first rest) =
  let viewSingle (Grammar var syntax) =
        row [] <|
          viewStart var :: (List.intersperse viewDeliniator <| List.map viewSyntax syntax)

      viewStart var =
        row [] [ el [ asItalic ] (text var), el [] (text " ::= ") ]

      viewDeliniator =
        el [] (text " | ")

      viewSyntax syntax =
        el [ asItalic ] (text syntax)
  in
  column [ paddingXY 0 20, centerX ] (List.map viewSingle (first :: rest))



-- VIEW ELEMENTS


asItalic : Attribute Msg
asItalic =
  family [ typeface "cmu_serifitalic", sansSerif ]

title : String -> Element Msg
title string =
  el [ paddingXY 0 40, centerX, size 35 ] (text string)


stepTitle : String -> Element Msg
stepTitle string =
  el [ paddingXY 0 30, El.alignLeft, size 25 ] (text string)
