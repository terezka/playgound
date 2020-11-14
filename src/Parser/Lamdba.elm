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

-- λa. a λb. b

pExpression : Parser () -> (Exp -> Exp) -> Parser Exp
pExpression ending finish =
  oneOf
    [ succeed identity
        |. symbol "("
        |> andThen (pExpression (symbol ")"))
    , succeed (\a b -> Exp [Var a, b])
        |. symbol "λ"
        |= variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }
        |. symbol "."
        |. spaces
        |> andThen (pExpression (succeed ()))
    , succeed Var
        |= variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }
    ]
    |> andThen (\exp ->
          succeed identity
            |. spaces
            |= oneOf
                [ succeed (\b -> Exp [exp, b])
                    |. symbol "||"
                    |. spaces
                    |> andThen (pExpression ending)
                , succeed (\b -> Exp [exp, b])
                    |> andThen (pExpression ending)
                , succeed exp
                    |. ending
                ]
       )
    |> map finish


