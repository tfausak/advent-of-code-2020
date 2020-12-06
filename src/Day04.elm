module Day04 exposing (part1, part2)

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra


type alias Passport =
    { byr : String
    , iyr : String
    , eyr : String
    , hgt : String
    , hcl : String
    , ecl : String
    , pid : String
    , cid : Maybe String
    }


part1 : String -> String
part1 string =
    string
        |> parsePassports
        |> List.length
        |> String.fromInt


part2 : String -> String
part2 string =
    string
        |> parsePassports
        |> List.Extra.count isValid
        |> String.fromInt


isValid : Passport -> Bool
isValid passport =
    List.all identity
        [ check passport.byr String.toInt (isBetween 1920 2002)
        , check passport.iyr String.toInt (isBetween 2010 2020)
        , check passport.eyr String.toInt (isBetween 2020 2030)
        , case splitAt -2 passport.hgt of
            ( hgt, "cm" ) ->
                check hgt String.toInt (isBetween 150 193)

            ( hgt, "in" ) ->
                check hgt String.toInt (isBetween 59 76)

            _ ->
                False
        , case String.uncons passport.hcl of
            Just ( '#', hcl ) ->
                String.all Char.isHexDigit hcl

            _ ->
                False
        , List.member passport.ecl [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
        , String.length passport.pid == 9
        , String.all Char.isDigit passport.pid
        ]


check : a -> (a -> Maybe b) -> (b -> Bool) -> Bool
check x f p =
    case f x of
        Nothing ->
            False

        Just y ->
            p y


isBetween : comparable -> comparable -> comparable -> Bool
isBetween lo hi x =
    lo <= x && x <= hi


splitAt : Int -> String -> ( String, String )
splitAt index string =
    ( String.slice 0 index string
    , String.slice index (String.length string) string
    )


parsePassports : String -> List Passport
parsePassports string =
    string
        |> String.split "\n\n"
        |> List.filterMap parsePassport


parsePassport : String -> Maybe Passport
parsePassport string =
    string
        |> parsePairs
        |> Dict.fromList
        |> dictToPassport


dictToPassport : Dict String String -> Maybe Passport
dictToPassport dict =
    Just Passport
        |> required dict "byr"
        |> required dict "iyr"
        |> required dict "eyr"
        |> required dict "hgt"
        |> required dict "hcl"
        |> required dict "ecl"
        |> required dict "pid"
        |> optional dict "cid"


required : Dict comparable v -> comparable -> Maybe (v -> b) -> Maybe b
required dict key =
    Maybe.Extra.andMap (Dict.get key dict)


optional : Dict comparable v -> comparable -> Maybe (Maybe v -> b) -> Maybe b
optional dict key =
    Maybe.Extra.andMap (Just (Dict.get key dict))


parsePairs : String -> List ( String, String )
parsePairs string =
    string
        |> String.words
        |> List.filterMap parsePair


parsePair : String -> Maybe ( String, String )
parsePair string =
    string
        |> String.split ":"
        |> listToTuple


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing
