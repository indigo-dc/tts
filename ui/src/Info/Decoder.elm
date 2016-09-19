-- elm-package install -- yes noredink/elm-decode-pipeline


module Info.Decoder exposing (..)

import Info.Model as Info exposing (Model)
import Json.Decode
import Json.Decode.Pipeline


decodeInfo : Json.Decode.Decoder Info.Model
decodeInfo =
    let
        asInfo : String -> String -> Bool -> String -> Result String Info.Model
        asInfo version redirect_path logged_in display_name =
            Ok (Info.Model version redirect_path logged_in display_name)
    in
        Json.Decode.Pipeline.decode asInfo
            |> Json.Decode.Pipeline.required "version" (Json.Decode.string)
            |> Json.Decode.Pipeline.required "redirect_path" (Json.Decode.string)
            |> Json.Decode.Pipeline.required "logged_in" (Json.Decode.bool)
            |> Json.Decode.Pipeline.required "display_name" (Json.Decode.string)
            |> Json.Decode.Pipeline.resolveResult
