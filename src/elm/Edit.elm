module Edit
    exposing
        ( view
        , update
        , EditMsg(..)
        )

import Html exposing (..)
import Html.Attributes exposing (class, value, selected)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utilities exposing (..)
import Types exposing (..)
import Json.Decode
import SelectList exposing (SelectList, before, after)


-- MODEL


type EditMsg
    = BeatName String
    | InstrumentName Instrument String
    | AddInstrument
    | RemoveInstrument Instrument
    | SaveChanges
    | SelectAudioSound Instrument String



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


updateBeatName : String -> Model -> Model
updateBeatName newName model =
    let
        oldPattern =
            SelectList.selected model

        newPattern =
            { oldPattern | name = newName }
    in
        model |> updateSelected newPattern


updateInstrumentName : Instrument -> String -> Model -> Model
updateInstrumentName instrument newName model =
    let
        rename currentInstrument =
            { currentInstrument | name = newName }

        instruments =
            updateIf
                (matchesId instrument)
                rename
                (SelectList.selected model).instruments

        oldPattern =
            SelectList.selected model

        newPattern =
            { oldPattern | instruments = instruments }
    in
        model |> updateSelected newPattern


updateInstrumentSound : Instrument -> Sample -> Model -> Model
updateInstrumentSound instrument newSound model =
    let
        updateSound currentInstrument =
            { currentInstrument | sound = newSound }

        instruments =
            updateIf
                (matchesId instrument)
                updateSound
                (SelectList.selected model).instruments

        oldPattern =
            SelectList.selected model

        newPattern =
            { oldPattern | instruments = instruments }
    in
        model |> updateSelected newPattern


addInstrument : Model -> Model
addInstrument model =
    let
        id =
            List.length (SelectList.selected model).instruments

        newInstruments =
            (SelectList.selected model).instruments ++ [ Instrument id "New" False Kick [] ]
    in
        model |> updateInstruments newInstruments


removeInstrument : Instrument -> Model -> Model
removeInstrument instrument model =
    let
        toKeep =
            (model |> SelectList.selected).instruments |> List.filter (\i -> i.id /= instrument.id)
    in
        model |> updateInstruments toKeep


saveChanges : Model -> Model
saveChanges model =
    let
        oldSelectedPattern =
            model |> SelectList.selected

        newSelectedPattern =
            { oldSelectedPattern | interactionMode = PlayMode }
    in
        SelectList.fromLists (before model) newSelectedPattern (after model)


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


update : EditMsg -> Model -> Model
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


viewInstrumentOption : Sample -> Instrument -> Html EditMsg
viewInstrumentOption audio instrument =
    let
        name =
            toString audio
    in
        Html.option [ value name, selected (audio == instrument.sound) ] [ text name ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Json.Decode.map tagger targetValue)


viewInstrumentEdits : Instrument -> Html EditMsg
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


viewButton : String -> String -> EditMsg -> Html EditMsg
viewButton name className action =
    p [ class "control" ]
        [ button
            [ class ("button " ++ className), onClick action ]
            [ text name ]
        ]


view : Model -> Html EditMsg
view model =
    let
        selectedPattern =
            model |> SelectList.selected
    in
        section [ class "beat__edit-container section" ]
            [ div [ class "field" ]
                [ label [ class "label is-medium" ] [ text "Pattern Name" ]
                , input
                    [ class "beat__edit-name input"
                    , value selectedPattern.name
                    , onInput BeatName
                    ]
                    []
                ]
            , div
                [ class "field" ]
                (label [ class "label is-medium" ] [ text "Instruments" ] :: List.map viewInstrumentEdits (SelectList.selected model).instruments)
            , div [ class "actions field is-grouped" ]
                [ viewButton "Save" "is-primary" SaveChanges
                , viewButton "Add" "" AddInstrument
                ]
            ]
