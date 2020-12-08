module Day08 exposing (part1, part2)

import Array exposing (Array)
import Set exposing (Set)


type Problem
    = InfiniteLoop Int
    | InvalidPointer Int


type alias State =
    { accumulator : Int
    , instructions : Array Instruction
    , pointer : Int
    , seen : Set Int
    }


type alias Instruction =
    { operation : Operation
    , argument : Int
    }


type Operation
    = Acc
    | Jmp
    | Nop


part1 : String -> String
part1 string =
    string
        |> parseInstructions
        |> makeState
        |> runProgram
        |> Tuple.second
        |> .accumulator
        |> String.fromInt


parseInstructions : String -> Array Instruction
parseInstructions string =
    string
        |> String.lines
        |> List.filterMap parseInstruction
        |> Array.fromList


parseInstruction : String -> Maybe Instruction
parseInstruction string =
    case String.words string of
        [ operation, argument ] ->
            Maybe.map2 Instruction
                (parseOperation operation)
                (parseArgument argument)

        _ ->
            Nothing


parseOperation : String -> Maybe Operation
parseOperation string =
    case string of
        "acc" ->
            Just Acc

        "jmp" ->
            Just Jmp

        "nop" ->
            Just Nop

        _ ->
            Nothing


parseArgument : String -> Maybe Int
parseArgument string =
    String.toInt string


makeState : Array Instruction -> State
makeState instructions =
    { accumulator = 0
    , instructions = instructions
    , pointer = 0
    , seen = Set.empty
    }


runProgram : State -> ( Problem, State )
runProgram state =
    case stepProgram state of
        Err problem ->
            ( problem, state )

        Ok newState ->
            runProgram newState


stepProgram : State -> Result Problem State
stepProgram state =
    if Set.member state.pointer state.seen then
        Err (InfiniteLoop state.pointer)

    else
        case Array.get state.pointer state.instructions of
            Nothing ->
                Err (InvalidPointer state.pointer)

            Just instruction ->
                Ok (executeInstruction instruction state)


executeInstruction : Instruction -> State -> State
executeInstruction instruction state =
    let
        newState =
            { state
                | pointer = state.pointer + 1
                , seen = Set.insert state.pointer state.seen
            }
    in
    case instruction.operation of
        Acc ->
            { newState | accumulator = state.accumulator + instruction.argument }

        Jmp ->
            { newState | pointer = state.pointer + instruction.argument }

        Nop ->
            newState


part2 : String -> String
part2 _ =
    "TODO day 8 part 2"
