module Messages exposing (..)

import CredentialList.Model as CredentialList exposing (Model)
import Info.Model as Info exposing (Model)
import ProviderList.Model as ProviderList exposing (Model)
import Secret.Model as Secret exposing (Model)
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
    | RequestFailed String
    | LoggedOut
    | LogoutFailed String
    | Request String
    | Requested Secret.Model
    | Revoke String
    | Revoked
    | RevokeFailed String
    | Logout
