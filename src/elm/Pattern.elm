module Pattern exposing (..)

import Types exposing (..)
import Utilities exposing (..)
import SelectList exposing (SelectList, before, after)


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
    [ noteFromId 1
    , noteFromId 2
    , noteFromId 3
    , noteFromId 4
    , noteFromId 5
    , noteFromId 6
    , noteFromId 7
    , noteFromId 8
    , noteFromId 9
    , noteFromId 10
    , noteFromId 11
    , noteFromId 12
    , noteFromId 13
    , noteFromId 14
    , noteFromId 15
    , noteFromId 16
    ]


updatePatternLengthFromInput : String -> Pattern -> Pattern
updatePatternLengthFromInput input pattern =
    case String.toInt input of
        Err _ ->
            pattern

        Ok patternLength ->
            pattern
                |> setPatternLength patternLength
                |> updateModelNotePositions


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
            updateIf
                (matchesId instrument)
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
    SelectList.fromLists (before model) pattern (after model)


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
