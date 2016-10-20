module Info.Model exposing (..)


type alias Model =
    { version : String
    , redirectPath : String
    , loggedIn : Bool
    , displayName : String
    }