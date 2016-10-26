module AccessToken.Model exposing (..)


type alias Model =
    { token : String
    }


initModel : Model
initModel =
    { token = "" }
