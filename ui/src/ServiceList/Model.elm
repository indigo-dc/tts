module ServiceList.Model exposing (..)

import Service.Model as Service exposing (Model, update_model)


type alias Model =
    { serviceList : List Service.Model }


update_services : Model -> Model
update_services model =
    let
        update_service service list =
            let
                newService =
                    Service.update_model service
            in
                List.append list [ newService ]

        newServiceList =
            List.foldl update_service [] model.serviceList
    in
        { model
            | serviceList = newServiceList
        }


initModel : Model
initModel =
    { serviceList = []
    }
