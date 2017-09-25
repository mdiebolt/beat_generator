{-
   Save to loco storage
   Load from loco storage
   Stylez
   UUID for instruments
-}


module Main exposing (init)

import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h1
        , h2
        , input
        , label
        , p
        , section
        , span
        , table
        , tbody
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, classList, type_, checked, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List
import String
import Utilities exposing (..)
import Types exposing (..)
import Edit
    exposing
        ( updateBeatName
        , updateInstrumentName
        , addInstrument
        , removeInstrument
        , saveChanges
        , viewEdit
        )
import Ports


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    let
        instruments =
            [ emptyInstrument "Hi hat"
            , emptyInstrument "Snare"
            , emptyInstrument "Kick"
            ]

        emptyModel =
            Model "New Pattern" instruments 16 120 False
    in
        emptyModel |> updateModelNotePositions



-- MODEL
-- imported from Types
-- UPDATE


toPortFormat : Model -> List (List String)
toPortFormat model =
    let
        formatNote note =
            case note.value of
                Rest ->
                    "-"

                Accent ->
                    ">"

                Hit ->
                    "x"

        groupNotes instrument =
            instrument.notes
                |> List.map formatNote

        beat =
            model.instruments
                |> List.map groupNotes
    in
        beat


reposition : Int -> Note -> Note
reposition i note =
    { note | position = i + 1 }


shuffleInstrumentNotes : Model -> ( Model, Cmd Msg )
shuffleInstrumentNotes model =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd model.instruments
    in
        model ! cmds


shuffleNotes : Instrument -> List Note -> Model -> ( Model, Cmd Msg )
shuffleNotes instrument shuffledNotes model =
    let
        rearrangeNotes currentInstrument =
            let
                updated =
                    List.indexedMap reposition shuffledNotes
            in
                { currentInstrument | notes = updated }

        updatedInstruments =
            updateIf
                (matches .name instrument)
                rearrangeNotes
                model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


shift : Model -> ( Model, Cmd Msg )
shift model =
    let
        lastToFirst list =
            case List.reverse list of
                [] ->
                    []

                lastReversed :: restReversed ->
                    lastReversed :: List.reverse restReversed

        shiftNotes instrument =
            let
                updated =
                    instrument.notes
                        |> lastToFirst
                        |> List.indexedMap reposition
            in
                { instrument | notes = updated }

        updatedInstruments =
            updateIf
                .selected
                shiftNotes
                model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


updateSelected : Instrument -> Model -> ( Model, Cmd Msg )
updateSelected instrument model =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }

        updatedInstruments =
            updateIf
                (matches .name instrument)
                toggleSelected
                model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


cycleNote : Instrument -> Note -> Model -> ( Model, Cmd Msg )
cycleNote instrument note model =
    let
        cycle currentNote =
            case currentNote.value of
                Rest ->
                    { currentNote | value = Hit }

                Accent ->
                    { currentNote | value = Rest }

                Hit ->
                    { currentNote | value = Accent }

        cycleInstrumentNotes currentInstrument =
            let
                updated =
                    updateIf
                        (matches .position note)
                        cycle
                        currentInstrument.notes
            in
                { currentInstrument | notes = updated }

        updatedInstruments =
            updateIf
                (matches .name instrument)
                cycleInstrumentNotes
                model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


updateInstruments : List Instrument -> Model -> Model
updateInstruments instruments model =
    { model | instruments = instruments }


updateSubdivision : String -> Model -> ( Model, Cmd Msg )
updateSubdivision newSubdivision model =
    case String.toInt newSubdivision of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            let
                newModel =
                    { model | slots = val }
            in
                newModel |> updateModelNotePositions


updateTempo : String -> Model -> ( Model, Cmd Msg )
updateTempo newTempo model =
    case String.toInt newTempo of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            let
                newModel =
                    { model | tempo = val }
            in
                ( newModel, Cmd.none )


editBeat : Model -> ( Model, Cmd Msg )
editBeat model =
    ( { model | editMode = True }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            model |> shuffleInstrumentNotes

        ShuffledNotes instrument shuffledNotes ->
            model |> shuffleNotes instrument shuffledNotes

        Shift ->
            model |> shift

        ToggleSelected instrument ->
            model |> updateSelected instrument

        CycleNote instrument note ->
            model |> cycleNote instrument note

        ChangeSubdivision newSubdivision ->
            model |> updateSubdivision newSubdivision

        ChangeTempo newTempo ->
            model |> updateTempo newTempo

        ChangeBeatName newName ->
            model |> updateBeatName newName

        ChangeInstrumentName instrument newName ->
            model |> updateInstrumentName instrument newName

        AddInstrument ->
            model |> addInstrument

        RemoveInstrument instrument ->
            model |> removeInstrument instrument

        EditBeat ->
            model |> editBeat

        SaveChanges ->
            model |> saveChanges

        Play ->
            let
                -- This is hacky. Passing the second arg using a tuple
                -- because ports can only accept a single argument
                portPlay =
                    (Ports.play ( (toPortFormat model), model.tempo ))
            in
                ( model, portPlay )



-- VIEW


noteType : Beat -> ( String, String )
noteType beat =
    case beat of
        Rest ->
            ( "-", "-" )

        Hit ->
            ( "-", "x" )

        Accent ->
            ( ">", "x" )


accentClass : Beat -> Attribute msg
accentClass beat =
    case beat of
        Rest ->
            class "beat__accent beat__accent--rest"

        Hit ->
            class "beat__accent beat__accent--hit"

        Accent ->
            class "beat__accent beat__accent--accent"


noteValue : Instrument -> Note -> Html Msg
noteValue instrument note =
    let
        ( accent, hit ) =
            noteType note.value
    in
        td
            [ class "beat__note-container"
            , onClick (CycleNote instrument note)
            ]
            [ div [ accentClass note.value ] [ text <| accent ]
            , div [ class "beat__note" ] [ text <| hit ]
            ]


viewAccentsAndPattern : Instrument -> Html Msg
viewAccentsAndPattern instrument =
    let
        beatSelector =
            th [ class "beat__selector" ]
                [ label [ class "checkbox" ]
                    [ input
                        [ class "beat__pattern-selector checkbox"
                        , type_ "checkbox"
                        , checked instrument.selected
                        , onClick (ToggleSelected instrument)
                        ]
                        []
                    , text instrument.name
                    ]
                ]

        values =
            instrument.notes
                |> List.map (noteValue instrument)
                |> (::) beatSelector
    in
        tr
            [ classList
                [ ( "beat__pattern-container", True )
                , ( "beat__pattern-container--selected", instrument.selected )
                ]
            ]
            values


viewCount : String -> Html Msg
viewCount count =
    td [ class "beat__note-container" ]
        [ div [ class "beat__note beat__count" ] [ text count ]
        ]


viewPatterns : Model -> Html Msg
viewPatterns model =
    let
        beatSelector =
            td [ class "beat__selector" ] []

        counts =
            [ "1"
            , "e"
            , "&"
            , "a"
            , "2"
            , "e"
            , "&"
            , "a"
            , "3"
            , "e"
            , "&"
            , "a"
            , "4"
            , "e"
            , "&"
            , "a"
            ]

        countHelper =
            counts
                |> List.map viewCount
                |> (::) beatSelector

        countHtml =
            tr [ class "beat__pattern-container" ] countHelper

        instrumentPatterns =
            model.instruments
                |> List.map viewAccentsAndPattern
                |> flip (++) [ countHtml ]
    in
        table [ class "beat__container" ]
            [ tbody [] instrumentPatterns
            ]


viewButtonIcon : String -> String -> Msg -> Html Msg
viewButtonIcon name icon action =
    p [ class "control" ]
        [ button [ class "button", onClick action ]
            [ span [ class "icon" ] [ Html.i [ class ("fa fa-" ++ icon) ] [] ]
            , span [] [ text name ]
            ]
        ]


viewButton : String -> Msg -> Html Msg
viewButton name action =
    p [ class "control" ]
        [ button
            [ class "button", onClick action ]
            [ text name ]
        ]


viewPlayButton : Model -> Html Msg
viewPlayButton model =
    let
        subdivision =
            toString model.slots

        tempo =
            toString model.tempo
    in
        div [ class "field has-addons" ]
            [ div [ class "control" ]
                [ button
                    [ class "button is-primary", onClick Play ]
                    [ text "Play" ]
                ]
            , div [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "number"
                    , onInput ChangeSubdivision
                    , value subdivision
                    ]
                    []
                ]
            , div [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "number"
                    , onInput ChangeTempo
                    , value tempo
                    ]
                    []
                ]
            ]


viewPlay : Model -> Html Msg
viewPlay model =
    let
        selectedActions =
            if List.any .selected model.instruments then
                div [ class "field is-grouped" ]
                    [ viewButtonIcon "Shuffle" "random" Shuffle
                    , viewButtonIcon "Shift" "angle-double-right" Shift
                    ]
            else
                text ""
    in
        div [ class "beat__play-container container" ]
            [ h1 [] [ text model.name ]
            , viewPatterns model
            , div [ class "field is-grouped" ]
                [ viewButton "Edit" EditBeat
                , selectedActions
                , viewPlayButton model
                ]
            ]


view : Model -> Html Msg
view model =
    let
        mode =
            if model.editMode then
                viewEdit model
            else
                viewPlay model
    in
        div []
            [ section [ class "hero" ]
                [ div [ class "hero-body" ]
                    [ div [ class "container" ]
                        [ h1 [ class "title" ] [ text "Beat Generator" ]
                        , h2 [ class "subtitle" ] [ text "Create permutations of drum patterns" ]
                        ]
                    ]
                ]
            , mode
            ]
