module Day01 exposing (main)

{-| --- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation
at a nice resort on a tropical island. Surely, Christmas will go on without
you.

The tropical island has its own currency and is entirely cash-only. The gold
coins used there have a little picture of a starfish; the locals just call them
stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay
the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense
report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
them together produces 1721 \* 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?

--- Part Two ---

The Elves in accounting are thankful for your help; one of them even offers you
a starfish coin they had left over from a past vacation. They offer you a
second one if you can find three numbers in your expense report that meet the
same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366,
and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to
2020?

-}

import Browser
import File
import File.Select
import Html
import Html.Events
import List.Extra
import Maybe.Extra
import Task


type alias Flags =
    ()


type alias Model =
    { input : String
    }


type Msg
    = FileLoaded String
    | FileRequested
    | FileSelected File.File
    | NoOp


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { input = "" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Advent of Code 2020" ]
        , Html.h2 [] [ Html.text "Day 1" ]
        , Html.div []
            [ Html.button
                [ Html.Events.onClick FileRequested ]
                [ Html.text "Select a file" ]
            ]
        , Html.div []
            [ Html.text "Part one: "
            , model.input
                |> part1
                |> Maybe.Extra.unwrap "unknown" String.fromInt
                |> Html.text
            ]
        , Html.div []
            [ Html.text "Part two: "
            , model.input
                |> part2
                |> Maybe.Extra.unwrap "unknown" String.fromInt
                |> Html.text
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileLoaded string ->
            ( { model | input = string }, Cmd.none )

        FileRequested ->
            ( model, File.Select.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


part1 : String -> Maybe Int
part1 input =
    input
        |> parse
        |> Maybe.map List.Extra.uniquePairs
        |> Maybe.andThen (List.Extra.find (\( x, y ) -> x + y == 2020))
        |> Maybe.map (\( x, y ) -> x * y)


parse : String -> Maybe (List Int)
parse input =
    input
        |> String.lines
        |> List.Extra.filterNot String.isEmpty
        |> Maybe.Extra.traverse String.toInt


part2 : String -> Maybe Int
part2 input =
    input
        |> parse
        |> Maybe.map uniqueTriples
        |> Maybe.andThen (List.Extra.find (\( x, y, z ) -> x + y + z == 2020))
        |> Maybe.map (\( x, y, z ) -> x * y * z)


uniqueTriples : List a -> List ( a, a, a )
uniqueTriples xs =
    case xs of
        [] ->
            []

        x :: ys ->
            List.map (\( y, z ) -> ( x, y, z )) (List.Extra.uniquePairs ys)
                ++ uniqueTriples ys
