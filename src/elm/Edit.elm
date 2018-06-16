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


updateInstruments : List Instrument -> Model -> Model
updateInstruments instruments model =
    { model | instruments = instruments }


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
                (matches .name instrument)
                rename
                model.instruments
    in
        { model | instruments = instruments }


updateInstrumentSound : Instrument -> AudioFile -> Model -> Model
updateInstrumentSound instrument newSound model =
    let
        updateSound currentInstrument =
            { currentInstrument | sound = newSound }

        instruments =
            updateIf
                (matches .name instrument)
                updateSound
                model.instruments
    in
        { model | instruments = instruments }


addInstrument : Model -> Model
addInstrument model =
    let
        newInstruments =
            model.instruments ++ [ Instrument "New" False Kick [] ]
    in
        model
            |> updateInstruments newInstruments


removeInstrument : Instrument -> Model -> Model
removeInstrument instrument model =
    let
        toKeep =
            model.instruments |> List.filter (\i -> i.name /= instrument.name)
    in
        model |> updateInstruments toKeep


saveChanges : Model -> Model
saveChanges model =
    { model | editMode = False }


selectAudioSound : String -> AudioFile
selectAudioSound name =
    case name of
        "HiHat" ->
            HiHat

        "Snare" ->
            Snare

        "Kick" ->
            Kick

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


viewInstrumentOption : AudioFile -> Instrument -> Html EditMsg
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
                    , viewInstrumentOption Snare instrument
                    , viewInstrumentOption Kick instrument
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
            [ Html.i [ class "fa fa-trash-o" ] []
            ]
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
            [ p [ class "control" ]
                [ button
                    [ class "button is-primary", onClick SaveChanges ]
                    [ text "Save" ]
                ]
            , p [ class "control" ]
                [ button
                    [ class "button", onClick AddInstrument ]
                    [ text "Add" ]
                ]
            ]
        ]
