{-
   Icon to represent instrument type
   Option to loop
   Keyboard shortcut for toggling values using left / right arrows and channel number
   Multiple pattern bugs – Refactor note management
   Match count helper with pattern length
   Highlight follows playback
   Edit note volume
   Don't play again if in progress
   Save / Load
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


noteFromId : Int -> Note
noteFromId id =
    (Note id Rest PointFive)


initialPattern : Pattern
initialPattern =
    let
        notes =
            [ noteFromId 1111
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


initialModel : Model
initialModel =
    { active = initialPattern
    , patterns = [ initialPattern ]
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- MODEL


type Msg
    = ShuffledNotes Instrument (List Note)
    | Shuffle
    | Shift
    | CycleNote Instrument Note
    | ToggleSelected Instrument
    | ChangePatternLength String
    | ChangeTempo String
    | Play
    | EditSub String
    | EnableEdit
    | EditMsg EditMsg
    | AddPattern
    | FocusPattern Pattern



-- UPDATE


updateModelNotePositionsWithCommand : Model -> ( Model, Cmd Msg )
updateModelNotePositionsWithCommand model =
    ( model |> updateModelNotePositions, Cmd.none )


formatNote : Note -> String
formatNote note =
    case note.value of
        Rest ->
            "-"

        Accent ->
            ">"

        Hit ->
            "x"


serialize : List Instrument -> List ( String, List String )
serialize instruments =
    let
        groupNotes instrument =
            let
                pair list =
                    ( toString instrument.sound, list )
            in
                instrument.notes
                    |> List.map formatNote
                    |> pair
    in
        instruments
            |> List.map groupNotes


shuffleInstrumentNotes : Model -> ( Model, Cmd Msg )
shuffleInstrumentNotes model =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd model.active.instruments
    in
        model ! cmds


updateInstrumentNotes : List Note -> Instrument -> Instrument
updateInstrumentNotes notes instrument =
    { instrument | notes = reposition notes }


shuffleNotes : Instrument -> List Note -> Model -> ( Model, Cmd Msg )
shuffleNotes instrument shuffledNotes ({ active } as model) =
    let
        updatedModel =
            updateIf
                (matchesId instrument)
                (updateInstrumentNotes shuffledNotes)
                active.instruments
                |> (updateInstrumentsInActiveModel model)
    in
        ( updatedModel, Cmd.none )



{- Take the last element of a List and move it to the front -}


wrap : List a -> List a
wrap xs =
    case List.reverse xs of
        [] ->
            []

        lastReversed :: restReversed ->
            lastReversed :: List.reverse restReversed


shift : Model -> ( Model, Cmd Msg )
shift ({ active } as model) =
    let
        shiftNotes instrument =
            updateInstrumentNotes (instrument.notes |> wrap) instrument

        updatedModel =
            updateIf
                .selected
                shiftNotes
                active.instruments
                |> (updateInstrumentsInActiveModel model)
    in
        ( updatedModel, Cmd.none )


updateSelected : Instrument -> Model -> ( Model, Cmd Msg )
updateSelected instrument ({ active } as model) =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }

        updatedModel =
            updateIf
                (matchesId instrument)
                toggleSelected
                model.active.instruments
                |> (updateInstrumentsInActiveModel model)
    in
        ( updatedModel, Cmd.none )


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


cycleNote : Instrument -> Note -> Model -> ( Model, Cmd Msg )
cycleNote instrument note ({ active } as model) =
    let
        updatedModel =
            updateIf
                (matchesId instrument)
                (cycleInstrumentNotes note)
                active.instruments
                |> (updateInstrumentsInActiveModel model)
    in
        ( updatedModel, Cmd.none )


updateInstrumentsInActiveModel : Model -> List Instrument -> Model
updateInstrumentsInActiveModel ({ active } as model) instruments =
    { model
        | active =
            { active
                | instruments = instruments
            }
    }


updatePatternLength : String -> Model -> ( Model, Cmd Msg )
updatePatternLength newPatternLength ({ active } as model) =
    case String.toInt newPatternLength of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            let
                newModel =
                    { model | active = { active | patternLength = val } }
            in
                newModel |> updateModelNotePositionsWithCommand


updateActiveSubdivision : Subdivision -> Model -> Model
updateActiveSubdivision subdivision ({ active } as model) =
    { model | active = { active | subdivision = subdivision } }


updateSub : String -> Model -> ( Model, Cmd Msg )
updateSub subString ({ active } as model) =
    case subString of
        "Sixteenth" ->
            ( model |> updateActiveSubdivision Sixteenth, Cmd.none )

        "Eighth" ->
            ( model |> updateActiveSubdivision Eighth, Cmd.none )

        "Quarter" ->
            ( model |> updateActiveSubdivision Quarter, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateTempo : String -> Model -> ( Model, Cmd Msg )
updateTempo newTempo ({ active } as model) =
    case String.toInt newTempo of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            ( { model | active = { active | tempo = val } }, Cmd.none )


addPattern : Model -> Model
addPattern ({ patterns } as model) =
    let
        updatedPatterns =
            patterns
                ++ [ initialPattern ]
                |> List.indexedMap (\i p -> { p | id = i })
    in
        { model | patterns = updatedPatterns }


editBeat : Model -> Model
editBeat model =
    let
        activePattern =
            model.active
    in
        { model | active = { activePattern | interactionMode = EditMode } }


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
                    (PlayPort.play ( (serialize model.active.instruments), model.active.tempo ))
            in
                ( model, portPlay )

        EditSub newSub ->
            model |> updateSub newSub

        AddPattern ->
            ( model |> addPattern, Cmd.none )

        FocusPattern pattern ->
            ( { model | active = pattern }, Cmd.none )



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
            [ div [ accentClass note.value ] [ text accent ]
            , div [ class "beat__note" ] [ text hit ]
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
            case model.active.subdivision of
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
            model.active.instruments
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


hasSelected : List { a | selected : Bool } -> Bool
hasSelected selectable =
    List.any .selected selectable


viewShuffle : Model -> Html Msg
viewShuffle model =
    if hasSelected model.active.instruments then
        viewButtonIcon "Shuffle" "random" Shuffle
    else
        text ""


viewShift : Model -> Html Msg
viewShift model =
    if List.any .selected model.active.instruments then
        viewButtonIcon "Shift" "angle-double-right" Shift
    else
        text ""


viewAddPattern : Html Msg
viewAddPattern =
    li [ onClick AddPattern ] [ a [] [ text "+" ] ]


viewPatternNameLinks : List Pattern -> Pattern -> List (Html Msg)
viewPatternNameLinks patterns active =
    let
        className pattern active =
            if pattern.id == active.id then
                "is-active"
            else
                ""

        link pattern =
            li
                [ class (className pattern active)
                , onClick (FocusPattern pattern)
                ]
                [ a [] [ text (pattern.name ++ toString (pattern.id)) ] ]
    in
        (List.map link patterns) ++ [ viewAddPattern ]


viewPatternNames : Model -> Html Msg
viewPatternNames { active, patterns } =
    div [ class "tabs" ]
        [ ul [] (viewPatternNameLinks patterns active) ]


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


viewPatternLengthInput : Int -> Html Msg
viewPatternLengthInput patternLength =
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
                        , value (toString patternLength)
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
                        , value (toString (model.active.tempo))
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewActiveSubdivision : Subdivision -> Subdivision -> Html Msg
viewActiveSubdivision activeSubdivision otherSubdivision =
    option
        [ selected (activeSubdivision == otherSubdivision) ]
        [ text (toString otherSubdivision) ]


viewCountHelper : Subdivision -> Html Msg
viewCountHelper subdivision =
    let
        options =
            [ Quarter, Eighth, Sixteenth ]
                |> List.map (viewActiveSubdivision subdivision)
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
        , a [ class "panel-block is-active" ] [ viewPatternLengthInput model.active.patternLength ]
        , a [ class "panel-block is-active" ] [ viewTempoInput model ]
        , a [ class "panel-block is-active" ] [ viewCountHelper model.active.subdivision ]
        ]


viewMode : Model -> Html Msg
viewMode model =
    case model.active.interactionMode of
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
