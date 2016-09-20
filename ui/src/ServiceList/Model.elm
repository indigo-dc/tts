module ServiceList.Model exposing (..)

import Service.Model as Service exposing (Model)


type alias Model =
    { serviceList : List Service.Model }


initModel : Model
initModel =
    { serviceList = []
    }
