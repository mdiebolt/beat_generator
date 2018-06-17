module Types exposing (..)

import Utilities exposing (fillWith)


type alias Model =
    { name : String
    , instruments : List Instrument
    , patternLength : Int
    , tempo : Int
    , playbackMode : Playback
    , interactionMode : InteractionMode
    , subdivision : Subdivision
    }


type alias Instrument =
    { id : Int
    , name : String
    , selected : Bool
    , sound : Sample
    , notes : List Note
    }


type alias Note =
    { position : Int
    , value : Beat
    , volume : Volume
    }


type Volume
    = Zero
    | PointOne
    | PointTwo
    | PointThree
    | PointFour
    | PointFive
    | PointSix
    | PointSeven
    | PointEight
    | PointNine
    | One


type InteractionMode
    = EditMode
    | PlayMode


type Playback
    = PlayOnce
    | PlayLoop


type Beat
    = Rest
    | Accent
    | Hit


type Sample
    = HiHat
    | Snare
    | Kick
    | Tom1
    | Tom2
    | Tom3
    | Tom4
    | Metronome



-- TODO: triplet subdivisions


type Subdivision
    = Sixteenth
    | Eighth
    | Quarter



-- TYPE SPECIFIC UTILITY


updateModelNotePositions : Model -> Model
updateModelNotePositions model =
    let
        updateInstrument instrument =
            let
                newNotes =
                    fillWith model.patternLength (Note 0 Rest PointFive) instrument.notes
            in
                { instrument | notes = newNotes }

        newInstruments =
            List.map updateInstrument model.instruments
    in
        { model | instruments = newInstruments }
