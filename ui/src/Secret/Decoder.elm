-- elm-package install -- yes noredink/elm-decode-pipeline


module Secret.Decoder exposing (decodeSecret)

import Json.Decode exposing (map, string, list, int, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Secret.Model as Secret exposing (Model, Credential, Entry)


decodeSecret : Decoder Secret.Model
decodeSecret =
    decode Secret.Model
        |> required "result" string
        |> optional "credential" decodeCredential { id = "", entries = [] }
        |> optional "oidc_login" decodeLogin { provider = "", url = "", msg = "" }
        |> optional "user_msg" string ""


decodeCredential : Decoder Secret.Credential
decodeCredential =
    decode Secret.Credential
        |> required "id" string
        |> required "entries" (list decodeEntry)


decodeLogin : Decoder Secret.OidcLogin
decodeLogin =
    decode Secret.OidcLogin
        |> required "provider" string
        |> required "url" string
        |> required "msg" string


decodeEntry : Decoder Secret.Entry
decodeEntry =
    decode Secret.Entry
        |> required "name" string
        |> required "type" string
        |> required "value" string
        |> optional "rows" int 30
        |> optional "cols" int 30
        |> optional "save_as" (map Just string) Nothing
