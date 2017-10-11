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
        |> Json.Decode.Pipeline.required "user_documentation" (Json.Decode.bool)
        |> Json.Decode.Pipeline.required "code_documentation" (Json.Decode.bool)
        |> Json.Decode.Pipeline.optional "service_request" (Json.Decode.map Just decodeServiceRequest) Nothing
        |> Json.Decode.Pipeline.optional "rsp_success" (Json.Decode.map Just Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.optional "rsp_error" (Json.Decode.map Just Json.Decode.string) Nothing


decodeServiceRequest : Json.Decode.Decoder Info.ServiceRequest
decodeServiceRequest =
    Json.Decode.Pipeline.decode Info.ServiceRequest
        |> Json.Decode.Pipeline.required "service" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "params" (Json.Decode.dict Json.Decode.value)
