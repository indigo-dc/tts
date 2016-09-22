-- elm-package install -- yes noredink/elm-decode-pipeline


module Service.Decoder exposing (..)

import Json.Decode exposing (Decoder, string, bool, int, list)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Service.Model as Service exposing (Model, Param)


decodeService : Decoder Service.Model
decodeService =
    decode Service.Model
        |> required "id" string
        |> required "description" string
        |> required "enabled" bool
        |> required "host" string
        |> required "port" string
        |> required "type" string
        |> required "cred_count" int
        |> required "cred_limit" int
        |> required "limit_reached" bool
        |> optional "params" (list decodeParam) []


decodeParam : Decoder Service.Param
decodeParam =
    decode Service.Param
        |> required "name" string
        |> required "value" string
        |> required "mandatory" bool
