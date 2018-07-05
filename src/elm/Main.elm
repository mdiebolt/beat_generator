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
import Utilities exposing (..)
import Types exposing (..)
import Edit
import PlayPort
import View exposing (view)
import Pattern exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


shuffleInstrumentNotes : Model -> Cmd Msg
shuffleInstrumentNotes model =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd (getInstruments model)
    in
        Cmd.batch cmds


updateInstrumentNotes : List Note -> Instrument -> Instrument
updateInstrumentNotes notes instrument =
    { instrument | notes = reposition notes }


shuffleNotes : Instrument -> List Note -> Model -> Model
shuffleNotes instrument shuffledNotes model =
    updateIf
        (matchesId instrument)
        (updateInstrumentNotes shuffledNotes)
        (getInstruments model)
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
            (getInstruments model)
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
        (getInstruments model)
        |> (updateInstrumentsInActiveModel model)


portPlay : Model -> Cmd msg
portPlay model =
    (PlayPort.play
        ( (PlayPort.serialize (getInstruments model))
        , (getTempo model)
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            ( model, model |> shuffleInstrumentNotes )

        ShuffledNotes instrument shuffledNotes ->
            ( model |> shuffleNotes instrument shuffledNotes, Cmd.none )

        Shift ->
            ( model |> shift, Cmd.none )

        ToggleSelected instrument ->
            ( model |> updateSelectedInstrument instrument, Cmd.none )

        CycleNote instrument note ->
            ( model |> cycleNote instrument note, Cmd.none )

        ChangePatternLength newPatternLength ->
            ( model |> updatePatternLengthFromInput newPatternLength, Cmd.none )

        ChangeTempo newTempo ->
            ( model |> updateTempoFromInput newTempo, Cmd.none )

        EnableEdit ->
            ( model |> updateInteractionModeFromInput EditMode, Cmd.none )

        EditMsg subMsg ->
            ( model |> Edit.update subMsg, Cmd.none )

        Play ->
            ( model, portPlay model )

        EditSub newSubdivision ->
            ( model |> updateSubdivisionFromInput newSubdivision, Cmd.none )

        AddPattern ->
            ( model |> addPattern, Cmd.none )

        FocusPattern pattern ->
            ( model |> focusPattern pattern, Cmd.none )
