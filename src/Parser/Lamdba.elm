module Parser.Lamdba exposing (..)

import Parser exposing (..)
import Set


type Exp
  = Lambda String Exp
  | Variable String
  | Application Exp Exp


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
    , succeed Lambda
        |. symbol "λ"
        |= variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }
        |. symbol "."
        |. spaces
        |> andThen (pExpression (succeed ()))
    , succeed Variable
        |= variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }
    ]
    |> andThen (\exp ->
          succeed identity
            |. spaces
            |= oneOf
                [ succeed (Application exp)
                    |. symbol "||"
                    |. spaces
                    |> andThen (pExpression ending)
                , succeed (Application exp)
                    |> andThen (pExpression ending)
                , succeed exp
                    |. ending
                ]
       )
    |> map finish



isWhitespace : Char -> Bool
isWhitespace c =
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
