module CredentialList.Model exposing (..)

import Credential.Model as Credential exposing (Model)


type alias Model =
    { credentialList : List Credential.Model }


initModel : Model
initModel =
    { credentialList = []
    }
