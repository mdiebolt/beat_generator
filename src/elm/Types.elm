module Types exposing (..)


type alias Model =
    { name : String
    , instruments : List Instrument
    , slots : Int
    , tempo : Int
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


type Beat
    = Rest
    | Accent
    | Hit
