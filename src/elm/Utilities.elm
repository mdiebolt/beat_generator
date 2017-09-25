module Utilities exposing (..)

import Types exposing (..)


updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item
            else
                item
        )
        list


matches : (a -> b) -> a -> a -> Bool
matches prop r toCompare =
    (prop r) == (prop toCompare)


createInstrumentNotes : Int -> Instrument -> Instrument
createInstrumentNotes targetLength instrument =
    let
        createEmptyNote position =
            Note position Rest

        existingNotes =
            instrument.notes

        numberOfExistingNotes =
            List.length existingNotes

        firstNewPosition =
            numberOfExistingNotes + 1

        newNotes =
            List.range firstNewPosition targetLength
                |> List.map createEmptyNote
    in
        { instrument
            | notes =
                List.take targetLength (existingNotes ++ newNotes)
        }


updateModelNotePositions : Model -> ( Model, Cmd Msg )
updateModelNotePositions model =
    let
        instruments =
            List.map (createInstrumentNotes model.slots) model.instruments
    in
        ( { model | instruments = instruments }, Cmd.none )


emptyInstrument : String -> Instrument
emptyInstrument name =
    Instrument name False []
