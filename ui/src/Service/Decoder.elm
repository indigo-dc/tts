-- elm-package install -- yes noredink/elm-decode-pipeline


module Service.Decoder exposing (..)

import Json.Decode exposing (Decoder, maybe, nullable, string, bool, int, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Service.Model as Service exposing (Model, Param)


decodeService : Decoder Service.Model
decodeService =
    decode Service.Model
        |> required "id" string
        |> required "description" string
        |> required "enabled" bool
        |> required "cred_count" int
        |> required "cred_limit" int
        |> required "limit_reached" bool
        |> required "authorized" bool
        |> required "authz_tooltip" string
        |> required "pass_access_token" bool
        |> optional "params" (list (list decodeParam)) [ [] ]
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded []
        |> hardcoded 0


decodeParam : Decoder Service.Param
decodeParam =
    decode Service.Param
        |> required "key" string
        |> required "name" string
        |> required "description" string
        |> required "type" string
        |> required "mandatory" bool
