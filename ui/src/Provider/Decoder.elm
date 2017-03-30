-- elm-package install -- yes noredink/elm-decode-pipeline


module Provider.Decoder exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Provider.Model as Provider exposing (Model)


decodeProvider : Json.Decode.Decoder Provider.Model
decodeProvider =
    Json.Decode.Pipeline.decode Provider.Model
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "desc" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "priority" (Json.Decode.int) 0
        |> Json.Decode.Pipeline.required "issuer" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ready" (Json.Decode.bool)
