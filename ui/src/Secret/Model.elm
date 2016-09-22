module Secret.Model exposing (..)


type alias Model =
    { entries : List Entry
    }


type alias Entry =
    { name : String
    , type' : String
    , value : String
    , rows : Int
    , cols : Int
    }
