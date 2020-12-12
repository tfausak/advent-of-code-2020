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
import Day12
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
      , day = 12
      , file = Nothing
      , part = 1
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Attribute.style "font-family" "sans-serif"
        , Attribute.style "line-height" "1.5em"
        , Attribute.style "margin" "1.5em auto"
        , Attribute.style "max-width" "40em"
        , Attribute.style "padding" "0 1.5em"
        ]
        [ Html.h1 [] [ Html.text "Advent of Code" ]
        , Html.p []
            [ Html.text "Day "
            , Html.input
                [ Attribute.max "25"
                , Attribute.min "1"
                , Event.onInput DaySelected
                , Attribute.type_ "number"
                , Attribute.value (String.fromInt model.day)
                ]
                []
            , Html.text " Part "
            , Html.input
                [ Attribute.max "2"
                , Attribute.min "1"
                , Event.onInput PartSelected
                , Attribute.type_ "number"
                , Attribute.value (String.fromInt model.part)
                ]
                []
            , Html.text " Input "
            , Html.button
                [ Event.onClick FileRequested ]
                [ case model.file of
                    Nothing ->
                        Html.text "Select a file"

                    Just file ->
                        Html.text (File.name file)
                ]
            , Html.text " Output "
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

                ( 12, 1 ) ->
                    Day12.part1 content

                ( 12, 2 ) ->
                    Day12.part2 content

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
