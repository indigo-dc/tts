-- elm-package install -- yes noredink/elm-decode-pipeline


module Credential.Decoder exposing (..)

import Credential.Model as Credential exposing (Model)
import Json.Decode
import Json.Decode.Pipeline


decodeCredential : Json.Decode.Decoder Credential.Model
decodeCredential =
    Json.Decode.Pipeline.decode Credential.Model
        |> Json.Decode.Pipeline.required "cred_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "interface" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ctime" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "service_id" (Json.Decode.string)
