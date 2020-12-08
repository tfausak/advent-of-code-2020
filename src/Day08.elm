module Day08 exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Event
import Dict exposing (Dict)
import Array exposing (Array)
import Html.Attributes as Attribute


type alias Instruction =
    { operation : Operation
    , argument : Int
    }


type Operation
    = Acc
    | Jmp
    | Nop


parseInstructions : String -> List Instruction
parseInstructions =
    String.lines >> List.filterMap parseInstruction


parseInstruction : String -> Maybe Instruction
parseInstruction string =
    case String.words string of
        [ operation, rawArgument ] ->
            case String.toInt rawArgument of
                Just argument ->
                    case operation of
                        "acc" ->
                            Just { operation = Acc, argument = argument }

                        "jmp" ->
                            Just { operation = Jmp, argument = argument }

                        "nop" ->
                            Just { operation = Nop, argument = argument }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


type alias Flags =
    ()


type alias Model =
    { input : String
    , instructions : Array Instruction
    , pointer : Int
    , accumulator : Int
    , order : Dict Int Int
    , count : Int
    }


type Msg
    = Run
    | Step
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
    initWith """
        nop +0
        acc +1
        jmp +4
        acc +3
        jmp -3
        acc -99
        acc +1
        jmp -4
        acc +6
        """

initWith : String -> (Model, Cmd Msg)
initWith input = pure
    { input = clean input
    , instructions = Array.fromList (parseInstructions input)
    , pointer = 0
    , accumulator = 0
    , order = Dict.empty
    , count = 1
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Advent of Code 2020 day 8 part 1" ]
        , Html.textarea [ Event.onInput UpdateInput ] [ Html.text model.input ]
        , Html.button [Event.onClick Step] [Html.text "Step"]
        , Html.button [Event.onClick Run] [Html.text "Run"]
        , Html.p []
            [ Html.text "Accumulator = "
            , Html.text (String.fromInt model.accumulator)
            ]
        , Html.p []
            [ case Dict.get model.pointer model.order of
                Nothing -> Html.text "not seen before"
                Just n -> Html.text ("instruction " ++ String.fromInt (model.pointer + 1) ++ " seen before at step " ++ String.fromInt n)
            ]
        , model.instructions
            |> Array.toList
            |> List.indexedMap (\ index instruction -> Html.li []
                [ Html.text <| case instruction.operation of
                    Acc -> "acc"
                    Jmp -> "jmp"
                    Nop -> "nop"
                , Html.text " "
                , Html.text (String.fromInt instruction.argument)
                , case Dict.get index model.order of
                    Nothing -> Html.text ""
                    Just order -> Html.text (" (" ++ String.fromInt order ++ ")")
                , if index == model.pointer then Html.text " <-" else Html.text ""
                ])
            |> Html.ol [Attribute.style "font-family" "monospace"]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run -> pure ( run model )
        Step -> pure (step model)
        UpdateInput input ->
            initWith input

run : Model -> Model
run model = if Dict.member model.pointer model.order
    then model
    else run (step model)

step : Model -> Model
step model = case Array.get model.pointer model.instructions of
    Nothing -> model
    Just instruction ->
        let
            newModel = { model | count = model.count + 1, pointer = model.pointer + 1
                , order = Dict.update model.pointer (\ m -> Just <| case m of
                    Nothing -> model.count
                    Just n -> n) model.order }
        in case instruction.operation of
            Acc -> { newModel | accumulator = model.accumulator + instruction.argument }
            Jmp -> { newModel | pointer = model.pointer + instruction.argument }
            Nop -> newModel


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


pure : model -> ( model, Cmd msg )
pure model =
    ( model, Cmd.none )


clean : String -> String
clean =
    String.lines
        >> List.map String.trim
        >> List.filter (not << String.isEmpty)
        >> String.join "\n"
