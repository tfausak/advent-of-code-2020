module Day04 exposing (part1, part2)

{-| <https://adventofcode.com/2020/day/4>


# Day 4: Passport Processing

You arrive at the airport only to realize that you grabbed your North Pole
Credentials instead of your passport. While these documents are extremely
similar, North Pole Credentials aren't issued by a country and therefore aren't
actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line
has formed for the automatic passport scanners, and the delay could upset your
travel itinerary.

Due to some questionable network security, you realize you might be able to
solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble
detecting which passports have all required fields. The expected fields are as
follows:

  - `byr` (Birth Year)
  - `iyr` (Issue Year)
  - `eyr` (Expiration Year)
  - `hgt` (Height)
  - `hcl` (Hair Color)
  - `ecl` (Eye Color)
  - `pid` (Passport ID)
  - `cid` (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is
represented as a sequence of key:value pairs separated by spaces or newlines.
Passports are separated by blank lines.

Here is an example batch file containing four passports:

    ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
    byr:1937 iyr:2017 cid:147 hgt:183cm

    iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
    hcl:#cfa07d byr:1929

    hcl:#ae17e1 iyr:2013
    eyr:2024
    ecl:brn pid:760753108 byr:1931
    hgt:179cm

    hcl:#cfa07d eyr:2025 pid:166559648
    iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport
is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks
like data from North Pole Credentials, not a passport at all! Surely, nobody
would mind if you made the system temporarily ignore missing cid fields. Treat
this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine,
but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

Count the number of valid passports - those that have all required fields.
Treat cid as optional. In your batch file, how many passports are valid?

-}

import Dict exposing (Dict)
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
part2 _ =
    "TODO day 4 part 2"


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
