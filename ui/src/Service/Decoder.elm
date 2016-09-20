-- elm-package install -- yes noredink/elm-decode-pipeline


module Service.Decoder exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Service.Model as Service exposing (Model)


decodeService : Json.Decode.Decoder Service.Model
decodeService =
    Json.Decode.Pipeline.decode Service.Model
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "enabled" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "host" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "port" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "type" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "cred_count" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "cred_limit" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "limit_reached" (Json.Decode.bool)
