module Secret.Model exposing (..)


type alias Model =
    { credential : List Entry
    }


type alias Entry =
    { name : String
    , type' : String
    , value : String
    , rows : String
    , cols : String
    }
