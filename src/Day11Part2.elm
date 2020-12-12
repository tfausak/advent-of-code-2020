module Day11Part2 exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List.Extra
import Task


type alias Grid a =
    Array (Array a)


type Seat
    = Floor
    | Empty
    | Occupied


type alias Flags =
    ()


type alias Model =
    { input : String
    , seats : Grid Seat
    }


type Msg
    = Step
    | UpdateInput String


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
    let
        input =
            clean
                """
                L.LL.LL.LL
                LLLLLLL.LL
                L.L.L..L..
                LLLL.LL.LL
                L.LL.LL.LL
                L.LLLLL.LL
                ..L.L.....
                LLLLLLLLLL
                L.LLLLLL.L
                L.LLLLL.LL
                """
    in
    pure { input = input, seats = toGrid input }


clean : String -> String
clean string =
    string
        |> String.trim
        |> String.lines
        |> List.map String.trim
        |> String.join "\n"


toGrid : String -> Grid Seat
toGrid string =
    let
        toRow line =
            line
                |> String.toList
                |> List.filterMap charToSeat
                |> Array.fromList
    in
    string
        |> String.lines
        |> List.map toRow
        |> Array.fromList


charToSeat : Char -> Maybe Seat
charToSeat char =
    case char of
        '.' ->
            Just Floor

        'L' ->
            Just Empty

        '#' ->
            Just Occupied

        _ ->
            Nothing


seatToChar : Seat -> Char
seatToChar seat =
    case seat of
        Floor ->
            '.'

        Empty ->
            'L'

        Occupied ->
            '#'


view : Model -> Html Msg
view model =
    H.div []
        [ H.text "advent of code year 2020 day 11 part 1"
        , viewInput model.input
        , H.button [ E.onClick Step ] [ H.text "Step" ]
        , H.text " occupied seat count "
        , model.seats
            |> Array.map (\row -> row |> Array.toList |> List.Extra.count isOccupied)
            |> Array.toList
            |> List.sum
            |> String.fromInt
            |> H.text
        , viewSeats model.seats
        ]


viewInput : String -> Html Msg
viewInput input =
    H.textarea
        [ E.onInput UpdateInput, A.style "display" "block" ]
        [ H.text input ]


viewSeats : Grid Seat -> Html Msg
viewSeats seats =
    let
        viewSeat seat =
            seat
                |> seatToChar
                |> String.fromChar
                |> H.text
                |> List.singleton
                |> H.td []

        viewRow row =
            row
                |> Array.toList
                |> List.map viewSeat
                |> H.tr []
    in
    seats
        |> Array.toList
        |> List.map viewRow
        |> H.table [ A.style "font-family" "monospace" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            pure { model | seats = step model.seats }

        UpdateInput input ->
            pure { model | seats = toGrid input, input = input }


step : Grid Seat -> Grid Seat
step seats =
    seats
        |> Array.indexedMap
            (\i row ->
                row
                    |> Array.indexedMap
                        (\j seat ->
                            case seat of
                                Floor ->
                                    Floor

                                Empty ->
                                    if List.any isOccupied (getNeighbors i j seats) then
                                        Empty

                                    else
                                        Occupied

                                Occupied ->
                                    if List.Extra.count isOccupied (getNeighbors i j seats) >= 5 then
                                        Empty

                                    else
                                        Occupied
                        )
            )


isOccupied : Seat -> Bool
isOccupied =
    (==) Occupied


getAt : Int -> Int -> Grid a -> Maybe a
getAt row column grid =
    grid
        |> Array.get row
        |> Maybe.andThen (Array.get column)


getNeighbors : Int -> Int -> Grid Seat -> List Seat
getNeighbors row column grid =
  directions
    |> List.filterMap (\ d -> findFirst d row column grid)

type Direction = N | NE | E | SE | S | SW | W | NW

directions : List Direction
directions = [ N, NE, E, SE, S, SW, W, NW ]

findFirst : Direction -> Int -> Int -> Grid Seat -> Maybe Seat
findFirst direction row column grid =
  let
    ( newRow, newColumn ) = case direction of
      N -> (row + 1, column )
      NE -> (row + 1, column + 1 )
      E -> (row, column + 1 )
      SE -> (row - 1, column + 1 )
      S -> (row - 1, column )
      SW -> (row - 1, column - 1 )
      W -> (row, column - 1 )
      NW -> (row  +1, column - 1 )
  in case getAt newRow newColumn grid of
    Just Floor -> findFirst direction newRow newColumn grid
    x -> x


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


pure : model -> ( model, Cmd msg )
pure model =
    ( model, Cmd.none )
