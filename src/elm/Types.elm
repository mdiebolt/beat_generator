module Types exposing (..)

import Utilities exposing (fillWith)


type alias Model =
    { name : String
    , instruments : List Instrument
    , patternLength : Int
    , tempo : Int
    , metronome : Bool
    , editMode : Bool
    , subdivision : Subdivision
    }


type alias Instrument =
    { name : String
    , selected : Bool
    , sound : AudioFile
    , notes : List Note
    }


type alias Note =
    { position : Int
    , value : Beat
    }


type Beat
    = Rest
    | Accent
    | Hit


type AudioFile
    = HiHat
    | Snare
    | Kick



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
                    fillWith model.patternLength (Note 0 Rest) instrument.notes
            in
                { instrument | notes = newNotes }

        newInstruments =
            List.map updateInstrument model.instruments
    in
        { model | instruments = newInstruments }
