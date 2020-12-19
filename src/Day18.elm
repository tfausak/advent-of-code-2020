module Day18 exposing (part1, part2)

import Parser exposing ((|.), (|=), Parser)


type alias Homework =
    List Expression


type alias Expression =
    List Token


type Token
    = Number Int
    | Operator Operator
    | Parenthesis Parenthesis


type Operator
    = Addition
    | Multiplication


type Parenthesis
    = Left
    | Right


part1 : String -> String
part1 =
    solveWith (always 0)


part2 : String -> String
part2 =
    solveWith <|
        \operator ->
            case operator of
                Addition ->
                    1

                Multiplication ->
                    0


solveWith : (Operator -> Int) -> String -> String
solveWith precedence string =
    case Parser.run (parseHomework |. Parser.end) string of
        Err deadEnds ->
            Parser.deadEndsToString deadEnds

        Ok homework ->
            homework
                |> List.filterMap
                    (shuntWith precedence [] []
                        >> List.reverse
                        >> evaluateWith []
                    )
                |> List.sum
                |> String.fromInt


evaluateWith : List Int -> Expression -> Maybe Int
evaluateWith stack expression =
    case ( expression, stack ) of
        ( [], [ x ] ) ->
            Just x

        ( (Number x) :: e, _ ) ->
            evaluateWith (x :: stack) e

        ( (Operator Addition) :: e, x :: y :: s ) ->
            evaluateWith ((x + y) :: s) e

        ( (Operator Multiplication) :: e, x :: y :: s ) ->
            evaluateWith ((x * y) :: s) e

        _ ->
            Nothing


shuntWith : (Operator -> Int) -> List Token -> List Token -> Expression -> Expression
shuntWith precedence output operators input =
    case input of
        [] ->
            List.reverse operators ++ output

        first :: rest ->
            let
                ( newTokens, newOperators ) =
                    case first of
                        Number _ ->
                            ( first :: output, operators )

                        Operator operator ->
                            shuntOperator precedence output operators operator

                        Parenthesis Left ->
                            ( output, first :: operators )

                        Parenthesis Right ->
                            shuntParenthesis output operators
            in
            shuntWith precedence newTokens newOperators rest


shuntOperator : (Operator -> Int) -> List Token -> List Token -> Operator -> ( List Token, List Token )
shuntOperator precedence output operators operator =
    case operators of
        [] ->
            ( output, Operator operator :: operators )

        first :: rest ->
            case first of
                Parenthesis Left ->
                    ( output, Operator operator :: operators )

                Operator x ->
                    if precedence x >= precedence operator then
                        shuntOperator precedence (first :: output) rest operator

                    else
                        ( output, Operator operator :: operators )

                _ ->
                    ( output, Operator operator :: operators )


shuntParenthesis : List Token -> List Token -> ( List Token, List Token )
shuntParenthesis output operators =
    case operators of
        [] ->
            ( output, operators )

        first :: rest ->
            case first of
                Parenthesis Left ->
                    ( output, rest )

                _ ->
                    shuntParenthesis (first :: output) rest


parseHomework : Parser Homework
parseHomework =
    parseSeparatedBy (Parser.token "\n") parseExpression


parseExpression : Parser Expression
parseExpression =
    parseSeparatedBy (Parser.chompWhile ((==) ' ')) parseToken


parseToken : Parser Token
parseToken =
    Parser.oneOf
        [ Parser.succeed Number |= Parser.int
        , Parser.succeed (Operator Addition) |. Parser.token "+"
        , Parser.succeed (Operator Multiplication) |. Parser.token "*"
        , Parser.succeed (Parenthesis Left) |. Parser.token "("
        , Parser.succeed (Parenthesis Right) |. Parser.token ")"
        ]


parseSeparatedBy : Parser separator -> Parser element -> Parser (List element)
parseSeparatedBy parseSeparator parseElement =
    let
        step elements =
            Parser.oneOf
                [ Parser.succeed (\element -> Parser.Loop (element :: elements))
                    |= parseElement
                    |. parseSeparator
                , Parser.succeed (Parser.Done (List.reverse elements))
                ]
    in
    Parser.loop [] step
