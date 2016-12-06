-- elm-package install -- yes noredink/elm-decode-pipeline


module ProviderList.Decoder exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Provider.Decoder as Provider exposing (decodeProvider)
import ProviderList.Model as ProviderList exposing (Model)


decodeProviderList : Json.Decode.Decoder ProviderList.Model
decodeProviderList =
    Json.Decode.Pipeline.decode ProviderList.Model
        |> Json.Decode.Pipeline.required "openid_provider_list" (Json.Decode.list Provider.decodeProvider)
