-- elm-package install -- yes noredink/elm-decode-pipeline


module AccessToken.Decoder exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)


decodeAccessToken : Decoder AccessToken.Model
decodeAccessToken =
    decode AccessToken.Model
        |> required "access_token" string
