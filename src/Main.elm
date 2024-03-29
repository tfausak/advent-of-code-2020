module Main exposing (main)

import Browser
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14Part1
import Day14Part2
import Day15
import Day16
import Day17Part1
import Day17Part2
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import File
import File.Select as Select
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Task


type alias Flags =
    ()


type alias Model =
    { content : Maybe String
    , day : Int
    , file : Maybe File.File
    , part : Int
    }


type Msg
    = DaySelected String
    | FileLoaded String
    | FileRequested
    | FileSelected File.File
    | PartSelected String


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
    ( { content = Nothing
      , day = 22
      , file = Nothing
      , part = 1
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Attribute.style "font-family" "sans-serif"
        , Attribute.style "line-height" "3em"
        , Attribute.style "margin" "3em"
        , Attribute.style "text-align" "center"
        ]
        [ Html.h1 [] [ Html.text "Advent of Code" ]
        , Html.h2 [] [ Html.text "2020" ]
        , Html.div []
            [ Html.text "Day "
            , Html.input
                [ Attribute.max "25"
                , Attribute.min "1"
                , Event.onInput DaySelected
                , Attribute.type_ "number"
                , Attribute.value (String.fromInt model.day)
                ]
                []
            ]
        , Html.div []
            [ Html.text " Part "
            , Html.input
                [ Attribute.max "2"
                , Attribute.min "1"
                , Event.onInput PartSelected
                , Attribute.type_ "number"
                , Attribute.value (String.fromInt model.part)
                ]
                []
            ]
        , Html.div []
            [ Html.text " Input "
            , Html.button
                [ Event.onClick FileRequested ]
                [ case model.file of
                    Nothing ->
                        Html.text "Select a file"

                    Just file ->
                        Html.text ("Selected: " ++ File.name file)
                ]
            ]
        , Html.div []
            [ Html.text " Output "
            , Html.input
                [ Attribute.readonly True
                , Attribute.value (solve model)
                ]
                []
            ]
        , Html.p []
            [ Html.a
                [ Attribute.href "https://github.com/tfausak/advent-of-code-2020" ]
                [ Html.text "github.com/tfausak/advent-of-code-2020" ]
            , Html.br [] []
            , Html.a
                [ Attribute.href "https://package.elm-lang.org" ]
                [ Html.text "package.elm-lang.org" ]
            ]
        ]


solve : Model -> String
solve model =
    case model.content of
        Nothing ->
            "no input"

        Just content ->
            case ( model.day, model.part ) of
                ( 1, 1 ) ->
                    Day01.part1 content

                ( 1, 2 ) ->
                    Day01.part2 content

                ( 2, 1 ) ->
                    Day02.part1 content

                ( 2, 2 ) ->
                    Day02.part2 content

                ( 3, 1 ) ->
                    Day03.part1 content

                ( 3, 2 ) ->
                    Day03.part2 content

                ( 4, 1 ) ->
                    Day04.part1 content

                ( 4, 2 ) ->
                    Day04.part2 content

                ( 5, 1 ) ->
                    Day05.part1 content

                ( 5, 2 ) ->
                    Day05.part2 content

                ( 6, 1 ) ->
                    Day06.part1 content

                ( 6, 2 ) ->
                    Day06.part2 content

                ( 7, 1 ) ->
                    Day07.part1 content

                ( 7, 2 ) ->
                    Day07.part2 content

                ( 8, 1 ) ->
                    Day08.part1 content

                ( 8, 2 ) ->
                    Day08.part2 content

                ( 9, 1 ) ->
                    Day09.part1 content

                ( 9, 2 ) ->
                    Day09.part2 content

                ( 10, 1 ) ->
                    Day10.part1 content

                ( 10, 2 ) ->
                    Day10.part2 content

                ( 11, 1 ) ->
                    Day11.part1 content

                ( 11, 2 ) ->
                    Day11.part2 content

                ( 12, 1 ) ->
                    Day12.part1 content

                ( 12, 2 ) ->
                    Day12.part2 content

                ( 13, 1 ) ->
                    Day13.part1 content

                ( 13, 2 ) ->
                    Day13.part2 content

                ( 14, 1 ) ->
                    Day14Part1.solve content

                ( 14, 2 ) ->
                    Day14Part2.solve content

                ( 15, 1 ) ->
                    Day15.part1 content

                ( 15, 2 ) ->
                    Day15.part2 content

                ( 16, 1 ) ->
                    Day16.part1 content

                ( 16, 2 ) ->
                    Day16.part2 content

                ( 17, 1 ) ->
                    Day17Part1.solve content

                ( 17, 2 ) ->
                    Day17Part2.solve content

                ( 18, 1 ) ->
                    Day18.part1 content

                ( 18, 2 ) ->
                    Day18.part2 content

                ( 19, 1 ) ->
                    Day19.part1 content

                ( 19, 2 ) ->
                    Day19.part2 content

                ( 20, 1 ) ->
                    Day20.part1 content

                ( 20, 2 ) ->
                    Day20.part2 content

                ( 21, 1 ) ->
                    Day21.part1 content

                ( 21, 2 ) ->
                    Day21.part2 content

                ( 22, 1 ) ->
                    Day22.part1 content

                ( 22, 2 ) ->
                    Day22.part2 content

                ( 23, 1 ) ->
                    Day23.part1 content

                ( 23, 2 ) ->
                    Day23.part2 content

                ( 24, 1 ) ->
                    Day24.part1 content

                ( 24, 2 ) ->
                    Day24.part2 content

                ( 25, 1 ) ->
                    Day25.part1 content

                ( 25, 2 ) ->
                    Day25.part2 content

                _ ->
                    "no solution"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DaySelected string ->
            case String.toInt string of
                Nothing ->
                    ( model, Cmd.none )

                Just int ->
                    ( { model | day = int }, Cmd.none )

        FileLoaded content ->
            ( { model | content = Just content }, Cmd.none )

        FileRequested ->
            ( model, Select.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( { model | content = Nothing, file = Just file }
            , Task.perform FileLoaded (File.toString file)
            )

        PartSelected string ->
            case String.toInt string of
                Nothing ->
                    ( model, Cmd.none )

                Just int ->
                    ( { model | part = int }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
