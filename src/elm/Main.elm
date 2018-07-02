{-
   Icon to represent instrument type
   Option to loop
   Keyboard shortcut for toggling values using left / right arrows and channel number
   Highlight follows playback
   Edit note volume
   Don't play again if in progress
   Save / Load
-}


module Main exposing (init)

import Html exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List
import String
import Utilities exposing (..)
import Types exposing (..)
import Edit
import PlayPort
import SelectList exposing (SelectList, before, after)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


patternFromId : Int -> Pattern
patternFromId id =
    { initialPattern | id = id }


initialPattern : Pattern
initialPattern =
    let
        notes =
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

        instruments =
            [ Instrument 0 "HiHat" False HiHat notes
            , Instrument 1 "Tom1" False Tom1 notes
            , Instrument 2 "Tom4" False Tom4 notes
            , Instrument 3 "Snare" False Snare notes
            , Instrument 4 "Kick" False Kick notes
            , Instrument 5 "Metronome" False Metronome notes
            ]
    in
        Pattern 0 "New Pattern" instruments 16 120 PlayOnce PlayMode Eighth


init : ( Model, Cmd Msg )
init =
    ( SelectList.singleton initialPattern, Cmd.none )



-- UPDATE


shuffleInstrumentNotes : Model -> ( Model, Cmd Msg )
shuffleInstrumentNotes model =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd (selectedInstruments model)
    in
        model ! cmds


updateInstrumentNotes : List Note -> Instrument -> Instrument
updateInstrumentNotes notes instrument =
    { instrument | notes = reposition notes }


shuffleNotes : Instrument -> List Note -> Model -> Model
shuffleNotes instrument shuffledNotes model =
    updateIf
        (matchesId instrument)
        (updateInstrumentNotes shuffledNotes)
        (selectedInstruments model)
        |> (updateInstrumentsInActiveModel model)



{- Take the last element of a List and move it to the front -}


wrap : List a -> List a
wrap xs =
    case List.reverse xs of
        [] ->
            []

        lastReversed :: restReversed ->
            lastReversed :: List.reverse restReversed


shift : Model -> Model
shift model =
    let
        shiftNotes instrument =
            updateInstrumentNotes (instrument.notes |> wrap) instrument
    in
        updateIf
            .selected
            shiftNotes
            (selectedInstruments model)
            |> (updateInstrumentsInActiveModel model)


selectedPattern : Model -> Pattern
selectedPattern model =
    SelectList.selected model


selectedInstruments : Model -> List Instrument
selectedInstruments model =
    (selectedPattern model).instruments


selectedTempo : Model -> Int
selectedTempo model =
    (selectedPattern model).tempo


updateSelectedInstrument : Instrument -> Model -> Model
updateSelectedInstrument instrument model =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }
    in
        updateIf
            (matchesId instrument)
            toggleSelected
            (selectedInstruments model)
            |> (updateInstrumentsInActiveModel model)


cycleNoteValue : Note -> Note
cycleNoteValue note =
    case note.value of
        Rest ->
            { note | value = Hit }

        Accent ->
            { note | value = Rest }

        Hit ->
            { note | value = Accent }


cycleInstrumentNotes : Note -> Instrument -> Instrument
cycleInstrumentNotes note instrument =
    let
        updated =
            updateIf
                (matches .position note)
                cycleNoteValue
                instrument.notes
    in
        { instrument | notes = updated }


cycleNote : Instrument -> Note -> Model -> Model
cycleNote instrument note model =
    updateIf
        (matchesId instrument)
        (cycleInstrumentNotes note)
        (selectedInstruments model)
        |> (updateInstrumentsInActiveModel model)


updateInstrumentsInPattern : Pattern -> List Instrument -> Pattern
updateInstrumentsInPattern pattern instruments =
    { pattern | instruments = instruments }


updateInstrumentsInActiveModel : Model -> List Instrument -> Model
updateInstrumentsInActiveModel model instruments =
    instruments
        |> updateInstrumentsInPattern (selectedPattern model)
        |> updatePatternInSelected model


updatePatternLength : String -> Model -> Model
updatePatternLength newPatternLength model =
    case String.toInt newPatternLength of
        Err _ ->
            model

        Ok val ->
            let
                oldPattern =
                    selectedPattern model

                newPattern =
                    { oldPattern | patternLength = val }
            in
                model
                    |> updateSelected newPattern
                    |> updateModelNotePositions


updateActiveSubdivision : Subdivision -> Model -> Model
updateActiveSubdivision subdivision model =
    let
        oldSelected =
            selectedPattern model

        newSelected =
            { oldSelected | subdivision = subdivision }
    in
        model |> updateSelected newSelected


updateSubdivision : String -> Model -> Model
updateSubdivision subdivisionInput model =
    case subdivisionInput of
        "Sixteenth" ->
            model |> updateActiveSubdivision Sixteenth

        "Eighth" ->
            model |> updateActiveSubdivision Eighth

        "Quarter" ->
            model |> updateActiveSubdivision Quarter

        _ ->
            model


updateTempo : String -> Model -> Model
updateTempo newTempo model =
    case String.toInt newTempo of
        Err _ ->
            model

        Ok val ->
            let
                oldSelected =
                    selectedPattern model

                newSelected =
                    { oldSelected | tempo = val }
            in
                model |> updateSelected newSelected


addPattern : Model -> Model
addPattern model =
    let
        newPattern =
            patternFromId (List.length (SelectList.toList model))
    in
        model
            |> SelectList.append [ newPattern ]
            |> focusPattern newPattern


focusPattern : Pattern -> Model -> Model
focusPattern pattern model =
    SelectList.select (matchesId pattern) model


updateSelected : Pattern -> Model -> Model
updateSelected pattern model =
    SelectList.fromLists (before model) pattern (after model)


updatePatternInSelected : Model -> Pattern -> Model
updatePatternInSelected =
    flip updateSelected


updateInteractionMode : InteractionMode -> Model -> Model
updateInteractionMode mode model =
    let
        oldPattern =
            selectedPattern model
    in
        model |> updateSelected ({ oldPattern | interactionMode = mode })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            model |> shuffleInstrumentNotes

        ShuffledNotes instrument shuffledNotes ->
            ( model |> shuffleNotes instrument shuffledNotes, Cmd.none )

        Shift ->
            ( model |> shift, Cmd.none )

        ToggleSelected instrument ->
            ( model |> updateSelectedInstrument instrument, Cmd.none )

        CycleNote instrument note ->
            ( model |> cycleNote instrument note, Cmd.none )

        ChangePatternLength newPatternLength ->
            ( model |> updatePatternLength newPatternLength, Cmd.none )

        ChangeTempo newTempo ->
            ( model |> updateTempo newTempo, Cmd.none )

        EnableEdit ->
            ( model |> updateInteractionMode EditMode, Cmd.none )

        EditMsg subMsg ->
            ( model |> Edit.update subMsg, Cmd.none )

        Play ->
            let
                -- This is hacky
                portPlay =
                    (PlayPort.play
                        ( (PlayPort.serialize (selectedInstruments model))
                        , (selectedTempo model)
                        )
                    )
            in
                ( model, portPlay )

        EditSub newSubdivision ->
            ( model |> updateSubdivision newSubdivision, Cmd.none )

        AddPattern ->
            ( model |> addPattern, Cmd.none )

        FocusPattern pattern ->
            ( model |> focusPattern pattern, Cmd.none )
