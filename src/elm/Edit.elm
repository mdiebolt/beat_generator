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


updateInstruments : List Instrument -> Model -> Model
updateInstruments instruments model =
    { model | instruments = resequenceInstruments instruments }


updateBeatName : String -> Model -> Model
updateBeatName newName model =
    { model | name = newName }


updateInstrumentName : Instrument -> String -> Model -> Model
updateInstrumentName instrument newName model =
    let
        rename currentInstrument =
            { currentInstrument | name = newName }

        instruments =
            updateIf
                (matches .id instrument)
                rename
                model.instruments
    in
        { model | instruments = instruments }


updateInstrumentSound : Instrument -> Sample -> Model -> Model
updateInstrumentSound instrument newSound model =
    let
        updateSound currentInstrument =
            { currentInstrument | sound = newSound }

        instruments =
            updateIf
                (matches .id instrument)
                updateSound
                model.instruments
    in
        { model | instruments = instruments }


addInstrument : Model -> Model
addInstrument model =
    let
        id =
            List.length model.instruments

        newInstruments =
            model.instruments ++ [ Instrument id "New" False Kick [] ]
    in
        model
            |> updateInstruments newInstruments


removeInstrument : Instrument -> Model -> Model
removeInstrument instrument model =
    let
        toKeep =
            model.instruments |> List.filter (\i -> i.id /= instrument.id)
    in
        model |> updateInstruments toKeep


saveChanges : Model -> Model
saveChanges model =
    { model | interactionMode = PlayMode }


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
    section [ class "beat__edit-container section" ]
        [ div [ class "field" ]
            [ label [ class "label is-medium" ] [ text "Pattern Name" ]
            , input
                [ class "beat__edit-name input"
                , value model.name
                , onInput BeatName
                ]
                []
            ]
        , div
            [ class "field" ]
            (label [ class "label is-medium" ] [ text "Instruments" ] :: List.map viewInstrumentEdits model.instruments)
        , div [ class "actions field is-grouped" ]
            [ viewButton "Save" "is-primary" SaveChanges
            , viewButton "Add" "" AddInstrument
            ]
        ]
