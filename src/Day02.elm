module Day02 exposing (part1, part2)

{-| <https://adventofcode.com/2020/day/2>


# Day 2: Password Philosophy

Your flight departs in a few days from the coastal airport; the easiest way
down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
"Something's wrong with our computers; we can't log in!" You ask if you can
take a look.

Their password database seems to be a little corrupted: some of the passwords
wouldn't have been allowed by the Official Toboggan Corporate Policy that was
in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of
passwords (according to the corrupted database) and the corporate policy when
that password was set.

For example, suppose you have the following list:

    1-3 a: abcde
    1-3 b: cdefg
    2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy
indicates the lowest and highest number of times a given letter must appear for
the password to be valid. For example, `1-3 a` means that the password must
contain a at least `1` time and at most `3` times.

In the above example, `2` passwords are valid. The middle password, `cdefg`, is
not; it contains no instances of `b`, but needs at least `1`. The first and
third passwords are valid: they contain one `a` or nine `c`, both within the
limits of their respective policies.

How many passwords are valid according to their policies?

-}

import List.Extra
import Parser exposing ((|.), (|=))
import Result.Extra
import Set
import String.Extra


part1 : String -> String
part1 string =
    string
        |> String.trimRight
        |> String.lines
        |> Result.Extra.combineMap (Parser.run parseLine)
        |> Result.Extra.unpack Parser.deadEndsToString
            (\lines ->
                lines
                    |> List.Extra.count isValid
                    |> String.fromInt
            )


isValid : Line -> Bool
isValid line =
    let
        count =
            String.Extra.countOccurrences (String.fromChar line.letter) line.password
    in
    line.lowest <= count && count <= line.highest


type alias Line =
    { lowest : Int
    , highest : Int
    , letter : Char
    , password : String
    }


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


part2 : String -> String
part2 _ =
    "TODO day 2 part 2"
