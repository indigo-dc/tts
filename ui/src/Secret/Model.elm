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


empty_credential : Credential
empty_credential =
    { id = "", entries = [] }


type alias Entry =
    { name : String
    , type_ : String
    , value : String
    , rows : Int
    , cols : Int
    , saveas : Maybe String
    }
