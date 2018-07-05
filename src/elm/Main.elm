{-
   Icon to represent instrument type
   Option to loop
   Keyboard shortcut for toggling values using left / right arrows and channel number
   Highlight follows playback
   Edit note volume
   Don't play again if in progress
   Save / Load
-}


module Main exposing (init)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (..)
import List
import Utilities
import Types exposing (..)
import InstrumentEditor exposing (InstrumentEditorMsg(..))
import PatternEditor exposing (PatternEditorMsg(..))
import Pattern exposing (..)
import SelectList
import PlayPort


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialModel : Model
initialModel =
    SelectList.singleton initialPattern


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = AddPattern
    | EnableEdit
    | Play
    | FocusPattern Pattern
    | InstrumentEditorMsg InstrumentEditorMsg
    | PatternEditorMsg PatternEditorMsg


patternsLength : Model -> Int
patternsLength model =
    List.length (SelectList.toList model)


addPattern : Model -> Model
addPattern model =
    let
        newPattern =
            patternFromId (patternsLength model)
    in
        model
            |> SelectList.append [ newPattern ]
            |> focusPattern newPattern


focusPattern : Pattern -> Model -> Model
focusPattern pattern model =
    SelectList.select (Utilities.matchesId pattern) model


updateInteractionModeFromInput : InteractionMode -> Model -> Model
updateInteractionModeFromInput mode model =
    let
        pattern =
            selectedPattern model

        newPattern =
            setInteractionMode mode pattern
    in
        setPattern newPattern model


portPlay : Model -> Cmd msg
portPlay model =
    let
        pattern =
            model |> selectedPattern
    in
        (PlayPort.play
            ( (PlayPort.serialize pattern.instruments)
            , (pattern.tempo)
            )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPattern ->
            ( model |> addPattern, Cmd.none )

        EnableEdit ->
            ( model |> updateInteractionModeFromInput EditMode, Cmd.none )

        FocusPattern pattern ->
            ( model |> focusPattern pattern, Cmd.none )

        Play ->
            ( model, portPlay model )

        -- Edit
        InstrumentEditorMsg subMsg ->
            let
                newPattern =
                    InstrumentEditor.update subMsg (selectedPattern model)
            in
                ( model |> setPattern newPattern, Cmd.none )

        -- Pattern Editor
        PatternEditorMsg subMsg ->
            let
                newPattern =
                    PatternEditor.update subMsg (selectedPattern model)
            in
                ( model |> setPattern newPattern, Cmd.none )



-- View


viewAddPattern : Html Msg
viewAddPattern =
    li [ Events.onClick AddPattern ] [ a [] [ text "+" ] ]


viewPatternNameLinks : Model -> List (Html Msg)
viewPatternNameLinks model =
    let
        className pattern =
            if Utilities.matchesId (selectedPattern model) pattern then
                "is-active"
            else
                ""

        link pattern =
            li
                [ class (className pattern)
                , Events.onClick (FocusPattern pattern)
                ]
                [ a [] [ text pattern.name ] ]

        links =
            SelectList.map link model
    in
        SelectList.toList links ++ [ viewAddPattern ]


viewPatternNames : Model -> Html Msg
viewPatternNames model =
    div [ class "tabs" ]
        [ ul [] (viewPatternNameLinks model) ]


viewButton : String -> String -> Msg -> Html Msg
viewButton name className action =
    p [ class "control" ]
        [ button
            [ class ("button " ++ className), Events.onClick action ]
            [ text name ]
        ]


viewPatternEditor : Pattern -> Html Msg
viewPatternEditor pattern =
    PatternEditor.view pattern
        |> Html.map PatternEditorMsg


viewPlay : Model -> Html Msg
viewPlay model =
    section [ class "beat__play-container section" ]
        [ viewPatternNames model
        , viewPatternEditor (model |> selectedPattern)
        , div [ class "field is-grouped" ]
            [ viewButton "Play" "is-primary" Play
            , viewButton "Edit" "" EnableEdit
            ]
        ]


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


viewInstrumentEditor : Pattern -> Html Msg
viewInstrumentEditor pattern =
    InstrumentEditor.view pattern
        |> Html.map InstrumentEditorMsg


viewMode : Model -> Html Msg
viewMode model =
    case (getInteractionMode (selectedPattern model)) of
        EditMode ->
            viewInstrumentEditor (selectedPattern model)

        PlayMode ->
            viewPlay model


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
