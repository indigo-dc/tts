module Provider.Model exposing (..)


type alias Model =
    { id : String
    , description : String
    , issuer : String
    , ready : Bool
    }


isReady : Model -> Bool
isReady model =
    model.ready
