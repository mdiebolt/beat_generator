{-
   Save / Load
   Icon to represent instrument type
   Option to loop
   Keyboard shortcut for toggling values using left / right arrows and channel number
   Multiple patterns
   Highlight follows playback
   Volume per track
   Don't play again if in progress
-}


module Main exposing (init)

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_, checked, selected, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List
import String
import Utilities exposing (..)
import Types exposing (..)
import Edit exposing (EditMsg(..))
import PlayPort


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
            [ Instrument 0 "HiHat" False HiHat []
            , Instrument 1 "Tom1" False Tom1 []
            , Instrument 2 "Tom4" False Tom4 []
            , Instrument 3 "Snare" False Snare []
            , Instrument 4 "Kick" False Kick []
            , Instrument 5 "Metronome" False Metronome []
            ]

        emptyModel =
            Model "New Pattern" instruments 16 120 PlayOnce PlayMode Eighth
    in
        emptyModel |> updateModelNotePositionsWithCommand



-- MODEL


type
    Msg
    -- Notes
    = ShuffledNotes Instrument (List Note)
    | Shuffle
    | Shift
      -- Note
    | CycleNote Instrument Note
      -- Instrument
    | ToggleSelected Instrument
      -- Playback
    | ChangePatternLength String
    | ChangeTempo String
    | Play
      -- Subdivision
    | EditSub String
      -- Edit
    | EnableEdit
    | EditMsg EditMsg



-- UPDATE


updateModelNotePositionsWithCommand : Model -> ( Model, Cmd Msg )
updateModelNotePositionsWithCommand model =
    ( model |> updateModelNotePositions, Cmd.none )


serialize : Model -> List ( String, List String )
serialize model =
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
            let
                pair list =
                    ( toString instrument.sound, list )
            in
                instrument.notes
                    |> List.map formatNote
                    |> pair

        beat =
            model.instruments
                |> List.map groupNotes
    in
        beat


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
            { currentInstrument | notes = reposition shuffledNotes }

        updatedInstruments =
            updateIf
                (matches .id instrument)
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
                        |> reposition
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
                (matches .id instrument)
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
                (matches .id instrument)
                cycleInstrumentNotes
                model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


updatePatternLength : String -> Model -> ( Model, Cmd Msg )
updatePatternLength newPatternLength model =
    case String.toInt newPatternLength of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            let
                newModel =
                    { model | patternLength = val }
            in
                newModel |> updateModelNotePositionsWithCommand


updateSub : String -> Model -> ( Model, Cmd Msg )
updateSub subString model =
    case subString of
        "Sixteenth" ->
            ( { model | subdivision = Sixteenth }, Cmd.none )

        "Eighth" ->
            ( { model | subdivision = Eighth }, Cmd.none )

        "Quarter" ->
            ( { model | subdivision = Quarter }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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


editBeat : Model -> Model
editBeat model =
    { model | interactionMode = EditMode }


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

        ChangePatternLength newPatternLength ->
            model |> updatePatternLength newPatternLength

        ChangeTempo newTempo ->
            model |> updateTempo newTempo

        EnableEdit ->
            ( model |> editBeat, Cmd.none )

        EditMsg subMsg ->
            let
                newModel =
                    model |> Edit.update subMsg
            in
                ( newModel, Cmd.none )

        Play ->
            let
                -- This is hacky
                portPlay =
                    (PlayPort.play ( (serialize model), model.tempo ))
            in
                ( model, portPlay )

        EditSub newSub ->
            model |> updateSub newSub



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


viewCount : Model -> String -> Html Msg
viewCount model count =
    td [ class "beat__note-container" ]
        [ div [ class "beat__note beat__count" ] [ text count ]
        ]


countMeasures : List String -> List String -> List String
countMeasures list filler =
    let
        beatCount number =
            number :: filler
    in
        List.concatMap beatCount list


viewPatterns : Model -> Html Msg
viewPatterns model =
    let
        beatSelector =
            td [ class "beat__selector" ] []

        countWithSubdivision =
            countMeasures [ "1", "2", "3", "4" ]

        counts =
            case model.subdivision of
                Sixteenth ->
                    countWithSubdivision [ "e", "&", "a" ]

                Eighth ->
                    countWithSubdivision [ "_", "&", "_" ]

                Quarter ->
                    countWithSubdivision [ "_", "_", "_" ]

        countHelper =
            counts
                |> List.map (viewCount model)
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


viewButton : String -> String -> Msg -> Html Msg
viewButton name className action =
    p [ class "control" ]
        [ button
            [ class ("button " ++ className), onClick action ]
            [ text name ]
        ]


viewShuffle : Model -> Html Msg
viewShuffle model =
    if List.any .selected model.instruments then
        viewButtonIcon "Shuffle" "random" Shuffle
    else
        text ""


viewShift : Model -> Html Msg
viewShift model =
    if List.any .selected model.instruments then
        viewButtonIcon "Shift" "angle-double-right" Shift
    else
        text ""


viewPatternNames : Model -> Html Msg
viewPatternNames model =
    div [ class "tabs" ]
        [ ul []
            [ li [ class "is-active" ]
                [ a [] [ text model.name ]
                ]
            , li []
                [ a [] [ text "+" ] ]
            ]
        ]


viewPlay : Model -> Html Msg
viewPlay model =
    section [ class "beat__play-container section" ]
        [ viewPatternNames model
        , viewPatterns model
        , div [ class "field is-grouped" ]
            [ viewButton "Play" "is-primary" Play
            , viewShuffle model
            , viewShift model
            , viewButton "Edit" "" EnableEdit
            ]
        , viewNavPanel model
        ]


viewNotesInput : Model -> Html Msg
viewNotesInput model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Notes" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , onInput ChangePatternLength
                        , value (toString model.patternLength)
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewTempoInput : Model -> Html Msg
viewTempoInput model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Tempo" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , onInput ChangeTempo
                        , value (toString (model.tempo))
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewCountHelper : Model -> Html Msg
viewCountHelper model =
    let
        subdivisionOption model sub =
            if model.subdivision == sub then
                option [ selected True ] [ text (toString sub) ]
            else
                option [] [ text (toString sub) ]

        options =
            [ Sixteenth, Eighth, Quarter ]
                |> List.map (subdivisionOption model)
    in
        div [ class "field is-horizontal" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label" ] [ text "Counts" ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ p [ class "control" ]
                        [ div [ class "select" ]
                            [ select [ onInput EditSub ] options
                            ]
                        ]
                    ]
                ]
            ]


viewNavPanel : Model -> Html Msg
viewNavPanel model =
    nav [ class "panel" ]
        [ p [ class "panel-heading" ] [ text "Settings" ]
        , a [ class "panel-block is-active" ] [ viewNotesInput model ]
        , a [ class "panel-block is-active" ] [ viewTempoInput model ]
        , a [ class "panel-block is-active" ] [ viewCountHelper model ]
        ]


viewMode : Model -> Html Msg
viewMode model =
    case model.interactionMode of
        EditMode ->
            Edit.view model
                |> Html.map EditMsg

        PlayMode ->
            viewPlay model


viewTitle : Html Msg
viewTitle =
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Beat Generator" ]
                , h2 [ class "subtitle" ] [ text "Create permutations of drum patterns" ]
                ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "container" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ strong [] [ text "Beat Generator" ]
                    , text " by "
                    , a [ Html.Attributes.href "http://diebo.lt" ] [ text "Matt Diebolt" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTitle
        , viewMode model
        , viewFooter
        ]
