module Parser.Lamdba exposing (..)

import Parser exposing (..)
import Set


type Exp
  = Exp (List Exp)
  | Var String


fromString : String -> Result String Exp
fromString string =
  run (pExpression end identity) string
    |> Result.mapError (Debug.log "error")
    |> Result.mapError deadEndsToString


pExpression : Parser () -> (Exp -> Exp) -> Parser Exp
pExpression ending finish =
  oneOf
    [ pSyntax (Exp []) [Symbol "(", Variable "e", Symbol ")"]
    , pSyntax (Exp []) [Symbol "λ", Variable "x", Symbol ". ", Variable "e"]
    , succeed Var
        |= variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }
    ]
    |> andThen (\exp ->
          oneOf
            [ pSyntax (Exp [exp]) [Symbol " ", Variable "e"]
            , succeed exp
                |. ending
            ]
       )
    |> map finish


type Syntax
  = Symbol String
  | Variable String


pSyntax : Exp -> List Syntax -> Parser Exp
pSyntax init syntax =
  let next some acc =
        case some of
          Symbol sym :: rest ->
            next rest (acc |. symbol sym)

          Variable var :: Symbol sym :: rest ->
            let subExp exp = pExpression (symbol sym) (addExp exp) in
            next rest (acc |> andThen subExp)

          Variable var :: rest ->
            let subExp exp = pExpression (succeed ()) (addExp exp) in
            next rest (acc |> andThen subExp)

          [] ->
            acc
  in
  next syntax (succeed init)


addExp : Exp -> Exp -> Exp
addExp exp1 exp =
  case exp1 of
    Exp list -> Exp (list ++ [exp])
    var -> var


