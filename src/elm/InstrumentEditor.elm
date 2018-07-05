module InstrumentEditor exposing (view, update)

import Html exposing (..)
import Html.Attributes exposing (class, value, selected)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utilities exposing (..)
import Types exposing (..)
import Json.Decode
import SelectList exposing (SelectList, before, after)


-- MODEL
-- UPDATE


resequenceInstruments : List Instrument -> List Instrument
resequenceInstruments instruments =
    List.indexedMap (\i instrument -> { instrument | id = i }) instruments


updateSelected : Pattern -> Model -> Model
updateSelected pattern model =
    SelectList.fromLists (before model) pattern (after model)


updateInstruments : List Instrument -> Model -> Model
updateInstruments instruments model =
    let
        oldPattern =
            SelectList.selected model

        newPattern =
            { oldPattern | instruments = resequenceInstruments instruments }
    in
        model |> updateSelected newPattern


updateBeatName : String -> Pattern -> Pattern
updateBeatName newName pattern =
    { pattern | name = newName }


updateInstrumentName : Instrument -> String -> Pattern -> Pattern
updateInstrumentName instrument newName pattern =
    let
        rename currentInstrument =
            { currentInstrument | name = newName }

        instruments =
            updateIf
                (matchesId instrument)
                rename
                pattern.instruments
    in
        { pattern | instruments = instruments }


updateInstrumentSound : Instrument -> Sample -> Pattern -> Pattern
updateInstrumentSound instrument newSound pattern =
    let
        updateSound currentInstrument =
            { currentInstrument | sound = newSound }

        newInstruments =
            updateIf
                (matchesId instrument)
                updateSound
                pattern.instruments
    in
        { pattern | instruments = newInstruments }


addInstrument : Pattern -> Pattern
addInstrument pattern =
    let
        id =
            List.length pattern.instruments

        newInstruments =
            pattern.instruments ++ [ Instrument id "New" False Kick [] ]
    in
        { pattern | instruments = newInstruments }


removeInstrument : Instrument -> Pattern -> Pattern
removeInstrument instrument pattern =
    let
        newInstruments =
            pattern.instruments |> List.filter (\i -> i.id /= instrument.id)
    in
        { pattern | instruments = newInstruments }


saveChanges : Pattern -> Pattern
saveChanges pattern =
    { pattern | interactionMode = PlayMode }


selectAudioSound : String -> Sample
selectAudioSound name =
    case name of
        "HiHat" ->
            HiHat

        "Tom1" ->
            Tom1

        "Tom2" ->
            Tom2

        "Tom3" ->
            Tom3

        "Tom4" ->
            Tom4

        "Snare" ->
            Snare

        "Kick" ->
            Kick

        "Metronome" ->
            Metronome

        _ ->
            Kick


update : InstrumentEditorMsg -> Pattern -> Pattern
update msg model =
    case msg of
        BeatName newName ->
            model |> updateBeatName newName

        InstrumentName instrument newName ->
            model |> updateInstrumentName instrument newName

        AddInstrument ->
            model
                |> addInstrument
                |> updateModelNotePositions

        RemoveInstrument instrument ->
            model |> removeInstrument instrument

        SaveChanges ->
            model |> saveChanges

        SelectAudioSound instrument name ->
            let
                newSound =
                    selectAudioSound name
            in
                model |> updateInstrumentSound instrument newSound



-- VIEW


viewInstrumentOption : Sample -> Instrument -> Html InstrumentEditorMsg
viewInstrumentOption audio instrument =
    let
        name =
            toString audio
    in
        Html.option [ value name, selected (audio == instrument.sound) ] [ text name ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Json.Decode.map tagger targetValue)


viewInstrumentEdits : Instrument -> Html InstrumentEditorMsg
viewInstrumentEdits instrument =
    div [ class "field control has-addons has-icons-right" ]
        [ p [ class "control" ]
            [ span [ class "select" ]
                [ select [ onChange (SelectAudioSound instrument) ]
                    [ viewInstrumentOption HiHat instrument
                    , viewInstrumentOption Tom1 instrument
                    , viewInstrumentOption Tom2 instrument
                    , viewInstrumentOption Tom3 instrument
                    , viewInstrumentOption Tom4 instrument
                    , viewInstrumentOption Snare instrument
                    , viewInstrumentOption Kick instrument
                    , viewInstrumentOption Metronome instrument
                    ]
                ]
            ]
        , input
            [ class "beat__edit-instrument input"
            , value instrument.name
            , onInput (InstrumentName instrument)
            ]
            []
        , span
            [ class "icon is-right beat__remove-instrument"
            , onClick (RemoveInstrument instrument)
            ]
            [ Html.i [ class "fa fa-trash" ] []
            ]
        ]


viewButton : String -> String -> InstrumentEditorMsg -> Html InstrumentEditorMsg
viewButton name className action =
    p [ class "control" ]
        [ button
            [ class ("button " ++ className), onClick action ]
            [ text name ]
        ]


view : Pattern -> Html InstrumentEditorMsg
view pattern =
    section [ class "beat__edit-container section" ]
        [ div [ class "field" ]
            [ label [ class "label is-medium" ] [ text "Pattern Name" ]
            , input
                [ class "beat__edit-name input"
                , value pattern.name
                , onInput BeatName
                ]
                []
            ]
        , div
            [ class "field" ]
            (label [ class "label is-medium" ] [ text "Instruments" ] :: List.map viewInstrumentEdits pattern.instruments)
        , div [ class "actions field is-grouped" ]
            [ viewButton "Save" "is-primary" SaveChanges
            , viewButton "Add" "" AddInstrument
            ]
        ]
