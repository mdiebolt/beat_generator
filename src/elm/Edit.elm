module Edit
    exposing
        ( view
        , update
        , EditMsg(..)
        )

import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Utilities exposing (..)
import Types exposing (..)


-- MODEL


type EditMsg
    = BeatName String
    | InstrumentName Instrument String
    | AddInstrument
    | RemoveInstrument Instrument
    | SaveChanges



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


addInstrument : Model -> Model
addInstrument model =
    let
        newInstruments =
            model.instruments ++ [ Instrument "New" False [] ]
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



-- VIEW


viewInstrumentEdits : Instrument -> Html EditMsg
viewInstrumentEdits instrument =
    div [ class "field control has-icons-right" ]
        [ input
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
    div [ class "beat__edit-container container" ]
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



-- DUPLICATED


createInstrumentNotes : Int -> Instrument -> Instrument
createInstrumentNotes targetLength instrument =
    let
        createEmptyNote position =
            Note position Rest

        existingNotes =
            instrument.notes

        numberOfExistingNotes =
            List.length existingNotes

        firstNewPosition =
            numberOfExistingNotes + 1

        newNotes =
            List.range firstNewPosition targetLength
                |> List.map createEmptyNote
    in
        { instrument
            | notes =
                List.take targetLength (existingNotes ++ newNotes)
        }


updateModelNotePositions : Model -> Model
updateModelNotePositions model =
    let
        instruments =
            List.map (createInstrumentNotes model.slots) model.instruments
    in
        { model | instruments = instruments }
