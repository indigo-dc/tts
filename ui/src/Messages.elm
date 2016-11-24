module Messages exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import CredentialList.Model as CredentialList exposing (Model)
import Info.Model as Info exposing (Model)
import ProviderList.Model as ProviderList exposing (Model)
import Secret.Model as Secret exposing (Model)
import Service.Model as Service exposing (Model)
import ServiceList.Model as ServiceList exposing (Model)


type Msg
    = Info Info.Model
    | InfoFailed String
    | ProviderList ProviderList.Model
    | ServiceList ServiceList.Model
    | Credential Secret.Model
    | CredentialList CredentialList.Model
    | ProviderListFailed String
    | ServiceListFailed String
    | CredentialListFailed String
    | RequestFailed Secret.Model
    | LoggedOut
    | LogoutFailed String
    | Request String
    | AdvancedRequest Service.Model
    | Requested Secret.Model
    | Revoke String
    | Revoked
    | HideAccessToken
    | HideSecret
    | RetrieveAccessToken
    | AccessTokenFailed String
    | AccessToken AccessToken.Model
    | RevokeFailed String
    | AdvancedCancel
    | AdvancedChange String String
    | Logout
