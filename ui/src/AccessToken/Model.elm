module AccessToken.Model exposing (..)


type alias Model =
    { token : String
    , issuer : String
    , issuer_id : String
    , subject : String
    }


initModel : Model
initModel =
    { token = ""
    , issuer = ""
    , issuer_id = ""
    , subject = ""
    }
