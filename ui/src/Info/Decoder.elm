-- elm-package install -- yes noredink/elm-decode-pipeline


module Info.Decoder exposing (..)

import Info.Model as Info exposing (Model)
import Json.Decode
import Json.Decode.Pipeline


decodeInfo : Json.Decode.Decoder Info.Model
decodeInfo =
    Json.Decode.Pipeline.decode Info.Model
        |> Json.Decode.Pipeline.required "version" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "redirect_path" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "logged_in" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "display_name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "error" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "issuer_id" (Json.Decode.string)
