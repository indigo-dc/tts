module CredentialList.Decoder exposing (..)

import Credential.Decoder as Credential exposing (decodeCredential)
import CredentialList.Model as CredentialList exposing (Model)
import Json.Decode
import Json.Decode.Pipeline


decodeCredentialList : Json.Decode.Decoder CredentialList.Model
decodeCredentialList =
    Json.Decode.Pipeline.decode CredentialList.Model
        |> Json.Decode.Pipeline.required "credential_list" (Json.Decode.list Credential.decodeCredential)
