module Pattern exposing (..)

import Types exposing (..)
import Utilities
import SelectList


patternFromId : Int -> Pattern
patternFromId id =
    { initialPattern | id = id }


initialPattern : Pattern
initialPattern =
    Pattern 0 "New Pattern" initialInstruments 16 120 PlayOnce PlayMode Eighth


initialInstruments : List Instrument
initialInstruments =
    [ Instrument 0 "HiHat" False HiHat initialNotes
    , Instrument 1 "Tom1" False Tom1 initialNotes
    , Instrument 2 "Tom4" False Tom4 initialNotes
    , Instrument 3 "Snare" False Snare initialNotes
    , Instrument 4 "Kick" False Kick initialNotes
    , Instrument 5 "Metronome" False Metronome initialNotes
    ]


initialNotes : List Note
initialNotes =
    [ Utilities.noteFromId 1
    , Utilities.noteFromId 2
    , Utilities.noteFromId 3
    , Utilities.noteFromId 4
    , Utilities.noteFromId 5
    , Utilities.noteFromId 6
    , Utilities.noteFromId 7
    , Utilities.noteFromId 8
    , Utilities.noteFromId 9
    , Utilities.noteFromId 10
    , Utilities.noteFromId 11
    , Utilities.noteFromId 12
    , Utilities.noteFromId 13
    , Utilities.noteFromId 14
    , Utilities.noteFromId 15
    , Utilities.noteFromId 16
    ]


updatePatternLengthFromInput : String -> Pattern -> Pattern
updatePatternLengthFromInput input pattern =
    case String.toInt input of
        Err _ ->
            pattern

        Ok patternLength ->
            pattern
                |> setPatternLength patternLength
                |> Utilities.updateModelNotePositions


updateSubdivisionFromInput : String -> Pattern -> Pattern
updateSubdivisionFromInput subdivisionInput pattern =
    case subdivisionInput of
        "Sixteenth" ->
            pattern |> setSubdivision Sixteenth

        "Eighth" ->
            pattern |> setSubdivision Eighth

        "Quarter" ->
            pattern |> setSubdivision Quarter

        _ ->
            pattern


updateTempoFromInput : String -> Pattern -> Pattern
updateTempoFromInput input pattern =
    case String.toInt input of
        Err _ ->
            pattern

        Ok tempo ->
            pattern |> setTempo tempo


updateSelectedInstrument : Instrument -> Pattern -> Pattern
updateSelectedInstrument instrument pattern =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }

        newInstruments =
            Utilities.updateIf
                (Utilities.matchesId instrument)
                toggleSelected
                (getInstruments pattern)
    in
        pattern |> setInstruments newInstruments



-- Helpers


selectedPattern : Model -> Pattern
selectedPattern model =
    model
        |> SelectList.selected



-- Getters


getSubdivision : Pattern -> Subdivision
getSubdivision pattern =
    pattern.subdivision


getInstruments : Pattern -> List Instrument
getInstruments pattern =
    pattern.instruments


getTempo : Pattern -> Int
getTempo pattern =
    pattern.tempo


getPatternLength : Pattern -> Int
getPatternLength pattern =
    pattern.patternLength


getInteractionMode : Pattern -> InteractionMode
getInteractionMode pattern =
    pattern.interactionMode



-- Setters


setPattern : Pattern -> Model -> Model
setPattern pattern model =
    SelectList.fromLists (SelectList.before model) pattern (SelectList.after model)


setInstruments : List Instrument -> Pattern -> Pattern
setInstruments instruments pattern =
    { pattern | instruments = instruments }


setPatternLength : Int -> Pattern -> Pattern
setPatternLength length pattern =
    { pattern | patternLength = length }


setSubdivision : Subdivision -> Pattern -> Pattern
setSubdivision subdivision pattern =
    { pattern | subdivision = subdivision }


setTempo : Int -> Pattern -> Pattern
setTempo tempo pattern =
    { pattern | tempo = tempo }


setInteractionMode : InteractionMode -> Pattern -> Pattern
setInteractionMode interactionMode pattern =
    { pattern | interactionMode = interactionMode }
