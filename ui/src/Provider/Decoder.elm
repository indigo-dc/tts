-- elm-package install -- yes noredink/elm-decode-pipeline


module Provider.Decoder exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Provider.Model as Provider exposing (Model)


decodeProvider : Json.Decode.Decoder Provider.Model
decodeProvider =
    Json.Decode.Pipeline.decode Provider.Model
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "issuer" (Json.Decode.string)
