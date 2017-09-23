{-
   Save to loco storage
   Load from loco storage
   Play with web audio (ports?)
   Stylez
   UUID for instruments
-}


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_, checked, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List
import String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


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
            (List.range firstNewPosition targetLength)
                |> List.map createEmptyNote
    in
        { instrument
            | notes =
                List.take targetLength (existingNotes ++ newNotes)
        }


resetModelInstruments : Model -> Model
resetModelInstruments model =
    let
        instruments =
            List.map (createInstrumentNotes model.slots) model.instruments
    in
        { model | instruments = instruments }


emptyInstrument : String -> Instrument
emptyInstrument name =
    Instrument name False []


init : ( Model, Cmd Msg )
init =
    let
        instruments =
            [ emptyInstrument "Hi hat"
            , emptyInstrument "Snare"
            , emptyInstrument "Kick"
            ]

        emptyModel =
            Model "New Pattern" instruments 16 False
    in
        ( resetModelInstruments emptyModel, Cmd.none )



-- MODEL


type alias Model =
    { name : String
    , instruments : List Instrument
    , slots : Int
    , editMode : Bool
    }


type alias Instrument =
    { name : String
    , selected : Bool
    , notes : List Note
    }


type alias Note =
    { position : Int
    , value : Beat
    }


type Msg
    = ShuffledNotes Instrument (List Note)
    | Shuffle
    | Shift
    | ToggleSelected Instrument
    | CycleNote Instrument Note
    | ChangeSubdivision String
    | ChangeBeatName String
    | ChangeInstrumentName Instrument String
    | AddInstrument
    | RemoveInstrument
    | EditBeat
    | SaveChanges


type Beat
    = Rest
    | Accent
    | Hit



-- UPDATE


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


shuffleNotes : Model -> Instrument -> List Note -> ( Model, Cmd Msg )
shuffleNotes model instrument shuffledNotes =
    let
        shuffleNotes toCompare instrument =
            if instrument.name == toCompare.name then
                { instrument | notes = List.indexedMap reposition shuffledNotes }
            else
                instrument

        updatedInstruments =
            List.map (shuffleNotes instrument) model.instruments
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
                    lastReversed :: (List.reverse restReversed)

        updateNotes instrument =
            instrument.notes
                |> lastToFirst
                |> List.indexedMap reposition

        shiftNotes instrument =
            if instrument.selected then
                { instrument | notes = updateNotes instrument }
            else
                instrument

        updatedInstruments =
            List.map shiftNotes model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


toggleSelected : Model -> Instrument -> ( Model, Cmd Msg )
toggleSelected model instrument =
    let
        select toCompare instrument =
            if instrument.name == toCompare.name then
                { instrument | selected = not instrument.selected }
            else
                instrument

        updatedInstruments =
            List.map (select instrument) model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


cycleNote : Model -> Instrument -> Note -> ( Model, Cmd Msg )
cycleNote model instrument note =
    let
        cycle toCompare note =
            if note.position == toCompare.position then
                case note.value of
                    Rest ->
                        { note | value = Hit }

                    Accent ->
                        { note | value = Rest }

                    Hit ->
                        { note | value = Accent }
            else
                note

        selectInstrument toCompare instrument =
            if instrument.name == toCompare.name then
                { instrument | notes = List.map (cycle note) instrument.notes }
            else
                instrument

        updatedInstruments =
            List.map (selectInstrument instrument) model.instruments
    in
        ( { model | instruments = updatedInstruments }, Cmd.none )


changeSubdivision : Model -> String -> ( Model, Cmd Msg )
changeSubdivision model newSubdivision =
    case String.toInt newSubdivision of
        Err _ ->
            ( model, Cmd.none )

        Ok val ->
            let
                newModel =
                    { model | slots = val }
            in
                ( resetModelInstruments newModel, Cmd.none )


changeBeatName : Model -> String -> ( Model, Cmd Msg )
changeBeatName model newName =
    ( { model | name = newName }, Cmd.none )


changeInstrumentName : Model -> Instrument -> String -> ( Model, Cmd Msg )
changeInstrumentName model instrument newName =
    let
        rename toCompare instrument =
            if toCompare.name == instrument.name then
                { instrument | name = newName }
            else
                instrument

        instruments =
            List.map (rename instrument) model.instruments
    in
        ( { model | instruments = instruments }, Cmd.none )


addInstrument : Model -> ( Model, Cmd Msg )
addInstrument model =
    let
        newInstruments =
            model.instruments ++ [ emptyInstrument "New" ]

        newModel =
            { model | instruments = newInstruments }
    in
        ( resetModelInstruments newModel, Cmd.none )


removeInstrument : Model -> ( Model, Cmd Msg )
removeInstrument model =
    let
        notSelected instrument =
            not instrument.selected

        instrumentsToKeep =
            List.filter notSelected model.instruments
    in
        ( { model | instruments = instrumentsToKeep }, Cmd.none )


editBeat : Model -> ( Model, Cmd Msg )
editBeat model =
    ( { model | editMode = True }, Cmd.none )


saveChanges : Model -> ( Model, Cmd Msg )
saveChanges model =
    ( { model | editMode = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            shuffleInstrumentNotes model

        ShuffledNotes instrument shuffledNotes ->
            shuffleNotes model instrument shuffledNotes

        Shift ->
            shift model

        ToggleSelected instrument ->
            toggleSelected model instrument

        CycleNote instrument note ->
            cycleNote model instrument note

        ChangeSubdivision newSubdivision ->
            changeSubdivision model newSubdivision

        ChangeBeatName newName ->
            changeBeatName model newName

        ChangeInstrumentName instrument newName ->
            changeInstrumentName model instrument newName

        AddInstrument ->
            addInstrument model

        RemoveInstrument ->
            removeInstrument model

        EditBeat ->
            editBeat model

        SaveChanges ->
            saveChanges model



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


noteValue : Instrument -> Note -> Html Msg
noteValue instrument note =
    let
        ( accent, hit ) =
            noteType note.value

        accentClass note =
            case note.value of
                Rest ->
                    "beat__accent beat__accent--rest"

                Hit ->
                    "beat__accent beat__accent--hit"

                Accent ->
                    "beat__accent beat__accent--accent"
    in
        td
            [ class "beat__note-container"
            , onClick (CycleNote instrument note)
            ]
            [ div [ class <| accentClass <| note ] [ text <| accent ]
            , div [ (class "beat__note") ] [ text <| hit ]
            ]


viewAccentsAndPattern : Instrument -> Html Msg
viewAccentsAndPattern instrument =
    let
        beatSelector =
            th [ class "beat__selector" ]
                [ label [ class "checkbox" ]
                    [ input
                        [ (class "beat__pattern-selector checkbox")
                        , (type_ "checkbox")
                        , (checked instrument.selected)
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
            (values)


viewCount : String -> Html Msg
viewCount count =
    td [ class "beat__note-container" ]
        [ div [ (class "beat__note beat__count") ] [ text count ]
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
                |> (List.map viewAccentsAndPattern)
                |> flip (++) [ countHtml ]
    in
        table [ class "beat__container" ]
            [ tbody [] instrumentPatterns
            ]


beatConfig : Int -> Html Msg
beatConfig setting =
    let
        val =
            toString setting
    in
        input
            [ class "beat__config input"
            , type_ "number"
            , onInput ChangeSubdivision
            , (value val)
            ]
            []


viewInstrumentEdits : Instrument -> Html Msg
viewInstrumentEdits instrument =
    div [ class "field" ]
        [ input
            [ class "beat__edit-instrument input"
            , value instrument.name
            , onInput (ChangeInstrumentName instrument)
            ]
            []
        ]


viewButtonIcon : String -> String -> Msg -> Html Msg
viewButtonIcon label icon action =
    p [ class "control" ]
        [ button [ class "button", onClick action ]
            [ span [ class "icon" ] [ i [ class ("fa fa-" ++ icon) ] [] ]
            , span [] [ text label ]
            ]
        ]


viewButton : String -> Msg -> Html Msg
viewButton label action =
    p [ class "control" ]
        [ button
            [ class "button"
            , onClick action
            ]
            [ text label ]
        ]


viewEdits : Model -> Html Msg
viewEdits model =
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
            (label [ class "label is-medium" ] [ text "Instruments" ] :: (List.map viewInstrumentEdits model.instruments))
        , div [ class "actions" ]
            [ viewButton "Save" SaveChanges ]
        ]


viewPlay : Model -> Html Msg
viewPlay model =
    div [ class "beat__play-container container" ]
        [ h1 [] [ text model.name ]
        , viewPatterns model
        , div [ class "actions field is-grouped" ]
            [ viewButton "Rename" EditBeat
            , viewButton "Add" AddInstrument
            , viewButton "Remove" RemoveInstrument
            , viewButtonIcon "Shuffle" "random" Shuffle
            , viewButtonIcon "Shift" "angle-double-right" Shift
            , (beatConfig model.slots)
            ]
        ]


view : Model -> Html Msg
view model =
    let
        mode =
            if model.editMode then
                viewEdits model
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
