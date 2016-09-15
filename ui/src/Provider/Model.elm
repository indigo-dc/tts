module Provider.Model exposing (..)


type alias Model =
    { id : String
    , issuer : String
    , description : String
    , ready : Bool
    }
