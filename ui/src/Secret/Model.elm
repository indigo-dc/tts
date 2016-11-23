module Secret.Model exposing (..)


type alias Model =
    { result : String
    , credential : Credential
    , error : String
    }


type alias Credential =
    { id : String
    , entries : List Entry
    }


type alias Entry =
    { name : String
    , type' : String
    , value : String
    , rows : Int
    , cols : Int
    }
