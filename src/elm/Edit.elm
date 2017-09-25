module Edit
    exposing
        ( updateBeatName
        , updateInstrumentName
        , addInstrument
        , removeInstrument
        , saveChanges
        , viewEdit
        )

import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Utilities exposing (..)
import Types exposing (..)


-- UPDATE


updateInstruments : List Instrument -> Model -> Model
updateInstruments instruments model =
    { model | instruments = instruments }


updateBeatName : String -> Model -> ( Model, Cmd Msg )
updateBeatName newName model =
    ( { model | name = newName }, Cmd.none )


updateInstrumentName : Instrument -> String -> Model -> ( Model, Cmd Msg )
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
        ( { model | instruments = instruments }, Cmd.none )


addInstrument : Model -> ( Model, Cmd Msg )
addInstrument model =
    let
        newInstruments =
            model.instruments ++ [ emptyInstrument "New" ]
    in
        model
            |> updateInstruments newInstruments
            |> updateModelNotePositions


removeInstrument : Instrument -> Model -> ( Model, Cmd Msg )
removeInstrument instrument model =
    let
        toKeep =
            model.instruments |> List.filter (\i -> i.name /= instrument.name)

        newModel =
            model |> updateInstruments toKeep
    in
        ( newModel, Cmd.none )


saveChanges : Model -> ( Model, Cmd Msg )
saveChanges model =
    ( { model | editMode = False }, Cmd.none )



-- VIEW


viewInstrumentEdits : Instrument -> Html Msg
viewInstrumentEdits instrument =
    div [ class "field control has-icons-right" ]
        [ input
            [ class "beat__edit-instrument input"
            , value instrument.name
            , onInput (ChangeInstrumentName instrument)
            ]
            []
        , span
            [ class "icon is-right beat__remove-instrument"
            , onClick (RemoveInstrument instrument)
            ]
            [ Html.i [ class "fa fa-trash-o" ] []
            ]
        ]


viewEdit : Model -> Html Msg
viewEdit model =
    div [ class "beat__edit-container container" ]
        [ div [ class "field" ]
            [ label [ class "label is-medium" ] [ text "Pattern Name" ]
            , input
                [ class "beat__edit-name input"
                , value model.name
                , onInput ChangeBeatName
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
