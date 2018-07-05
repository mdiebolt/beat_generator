module Utilities exposing (..)

import Types exposing (..)


updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    let
        filter item =
            if predicate item then
                update item
            else
                item
    in
        List.map filter list


matchesId : { a | id : Int } -> { a | id : Int } -> Bool
matchesId item toCompare =
    matches .id item toCompare


matches : (a -> b) -> a -> a -> Bool
matches prop r toCompare =
    (prop r) == (prop toCompare)


reposition : List { a | position : Int } -> List { a | position : Int }
reposition list =
    let
        updatePosition i a =
            { a | position = i + 1 }
    in
        List.indexedMap updatePosition list


fillWith :
    Int
    -> { a | position : Int }
    -> List { a | position : Int }
    -> List { a | position : Int }
fillWith length whenEmpty list =
    let
        newItems =
            List.repeat length whenEmpty
    in
        (list ++ newItems)
            |> reposition
            |> List.take length


noteFromId : Int -> Note
noteFromId id =
    (Note id Rest Five)


updateModelNotePositions : Pattern -> Pattern
updateModelNotePositions pattern =
    let
        updateInstrument instrument =
            let
                newNotes =
                    fillWith pattern.patternLength (noteFromId 0) instrument.notes
            in
                { instrument | notes = newNotes }

        newInstruments =
            List.map updateInstrument pattern.instruments
    in
        { pattern | instruments = newInstruments }
