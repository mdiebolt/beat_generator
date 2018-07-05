module Pattern exposing (..)

import Types exposing (..)
import Utilities
    exposing
        ( noteFromId
        , matchesId
        , updateIf
        , updateModelNotePositions
        )
import SelectList exposing (SelectList, before, after)


patternFromId : Int -> Pattern
patternFromId id =
    { initialPattern | id = id }


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


initialPattern : Pattern
initialPattern =
    let
        instruments =
            [ Instrument 0 "HiHat" False HiHat initialNotes
            , Instrument 1 "Tom1" False Tom1 initialNotes
            , Instrument 2 "Tom4" False Tom4 initialNotes
            , Instrument 3 "Snare" False Snare initialNotes
            , Instrument 4 "Kick" False Kick initialNotes
            , Instrument 5 "Metronome" False Metronome initialNotes
            ]
    in
        Pattern 0 "New Pattern" instruments 16 120 PlayOnce PlayMode Eighth


initialModel : Model
initialModel =
    SelectList.singleton initialPattern


patternsLength : Model -> Int
patternsLength model =
    List.length (SelectList.toList model)


addPattern : Model -> Model
addPattern model =
    let
        newPattern =
            patternFromId (patternsLength model)
    in
        model
            |> SelectList.append [ newPattern ]
            |> focusPattern newPattern


focusPattern : Pattern -> Model -> Model
focusPattern pattern model =
    SelectList.select (matchesId pattern) model


updatePatternLengthFromInput : String -> Model -> Model
updatePatternLengthFromInput newPatternLength model =
    case String.toInt newPatternLength of
        Err _ ->
            model

        Ok val ->
            model
                |> setPatternLength (selectedPattern model) val
                |> updateModelNotePositions


updateSubdivisionFromInput : String -> Model -> Model
updateSubdivisionFromInput subdivisionInput model =
    let
        pattern =
            selectedPattern model
    in
        case subdivisionInput of
            "Sixteenth" ->
                model |> setSubdivision pattern Sixteenth

            "Eighth" ->
                model |> setSubdivision pattern Eighth

            "Quarter" ->
                model |> setSubdivision pattern Quarter

            _ ->
                model


updateTempoFromInput : String -> Model -> Model
updateTempoFromInput input model =
    case String.toInt input of
        Err _ ->
            model

        Ok tempo ->
            model |> setTempo (selectedPattern model) tempo


updateInteractionModeFromInput : InteractionMode -> Model -> Model
updateInteractionModeFromInput mode model =
    model |> setInteractionMode (selectedPattern model) mode


updateSelectedInstrument : Instrument -> Model -> Model
updateSelectedInstrument instrument model =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }
    in
        updateIf
            (matchesId instrument)
            toggleSelected
            (getInstruments model)
            |> (updateInstrumentsInActiveModel model)



-- Getters


selectedPattern : Model -> Pattern
selectedPattern model =
    SelectList.selected model


getInstruments : Model -> List Instrument
getInstruments model =
    (selectedPattern model).instruments


getTempo : Model -> Int
getTempo model =
    (selectedPattern model).tempo



-- Setters


setPattern : Pattern -> Model -> Model
setPattern pattern model =
    SelectList.fromLists (before model) pattern (after model)



-- TODO: Clean this up


updateInstrumentsInActiveModel : Model -> List Instrument -> Model
updateInstrumentsInActiveModel model instruments =
    model
        |> setInstruments (selectedPattern model) instruments


setInstruments : Pattern -> List Instrument -> Model -> Model
setInstruments pattern instruments =
    setPattern { pattern | instruments = instruments }


setPatternLength : Pattern -> Int -> Model -> Model
setPatternLength pattern length =
    setPattern { pattern | patternLength = length }


setSubdivision : Pattern -> Subdivision -> Model -> Model
setSubdivision pattern subdivision =
    setPattern { pattern | subdivision = subdivision }


setTempo : Pattern -> Int -> Model -> Model
setTempo pattern tempo =
    setPattern { pattern | tempo = tempo }


setInteractionMode : Pattern -> InteractionMode -> Model -> Model
setInteractionMode pattern interactionMode =
    setPattern { pattern | interactionMode = interactionMode }
