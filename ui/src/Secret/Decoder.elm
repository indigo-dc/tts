-- elm-package install -- yes noredink/elm-decode-pipeline


module Secret.Decoder exposing (decodeSecret)

import Json.Decode exposing (string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Secret.Model as Secret exposing (Model, Entry)


decodeSecret : Decoder Secret.Model
decodeSecret =
    decode Secret.Model
        |> required "credential" (list decodeEntry)


decodeEntry : Decoder Secret.Entry
decodeEntry =
    decode Secret.Entry
        |> required "name" string
        |> required "type" string
        |> required "value" string
        |> optional "rows" string "30"
        |> optional "cols" string "30"
