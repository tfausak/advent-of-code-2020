module Day02 exposing (part1, part2)

import List.Extra
import Parser exposing ((|.), (|=))
import Result.Extra
import Set
import String.Extra


type alias Line =
    { lowest : Int
    , highest : Int
    , letter : Char
    , password : String
    }


withLines : String -> (List Line -> String) -> String
withLines string callback =
    string
        |> String.trimRight
        |> String.lines
        |> Result.Extra.combineMap (Parser.run parseLine)
        |> Result.Extra.unpack Parser.deadEndsToString callback


parseLine : Parser.Parser Line
parseLine =
    Parser.succeed Line
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
        |. Parser.symbol " "
        |= parseChar
        |. Parser.symbol ": "
        |= parseString
        |. Parser.end


parseChar : Parser.Parser Char
parseChar =
    let
        stringToChar string =
            case String.uncons string of
                Nothing ->
                    Parser.problem "empty string"

                Just ( char, _ ) ->
                    Parser.succeed char
    in
    Parser.variable
        { start = always True
        , inner = always False
        , reserved = Set.empty
        }
        |> Parser.andThen stringToChar


parseString : Parser.Parser String
parseString =
    Parser.variable
        { start = always True
        , inner = always True
        , reserved = Set.empty
        }


part1 : String -> String
part1 string =
    let
        count line =
            String.Extra.countOccurrences (String.fromChar line.letter) line.password

        isValid line =
            line.lowest <= count line && count line <= line.highest
    in
    withLines string
        (\lines ->
            lines
                |> List.Extra.count isValid
                |> String.fromInt
        )


part2 : String -> String
part2 string =
    let
        isValid line =
            xor
                (charAt (line.lowest - 1) line.password == Just line.letter)
                (charAt (line.highest - 1) line.password == Just line.letter)
    in
    withLines string
        (\lines ->
            lines
                |> List.Extra.count isValid
                |> String.fromInt
        )


charAt : Int -> String -> Maybe Char
charAt index string =
    string
        |> String.toList
        |> List.Extra.getAt index
