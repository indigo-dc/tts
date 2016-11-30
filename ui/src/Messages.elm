module Messages exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import CredentialList.Model as CredentialList exposing (Model)
import Http
import Info.Model as Info exposing (Model)
import ProviderList.Model as ProviderList exposing (Model)
import Secret.Model as Secret exposing (Model)
import Service.Model as Service exposing (Model)
import ServiceList.Model as ServiceList exposing (Model)


type Msg
    = Info (Result Http.Error Info.Model)
    | ProviderList (Result Http.Error ProviderList.Model)
    | ServiceList (Result Http.Error ServiceList.Model)
      -- | Credential (Result Http.Error Secret.Model)
    | CredentialList (Result Http.Error CredentialList.Model)
    | Requested (Result Http.Error Secret.Model)
    | AccessToken (Result Http.Error AccessToken.Model)
    | LoggedOut (Result Http.Error String)
    | Request String
    | AdvancedRequest Service.Model
    | Revoke String
    | Revoked (Result Http.Error Secret.Model)
    | HideAccessToken
    | HideSecret
    | RetrieveAccessToken
    | AdvancedCancel
    | AdvancedChange String String
    | Logout
