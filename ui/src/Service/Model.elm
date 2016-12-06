module Service.Model exposing (Model, Set, Param, update_model, setPos, hasBasic, hasAdvanced)


type alias Model =
    { id : String
    , description : String
    , enabled : Bool
    , credCount : Int
    , credLimit : Int
    , limitReached : Bool
    , parameter_sets : List Set
    , has_empty_set : Maybe Bool
    , has_non_empty_sets : Maybe Bool
    , non_empty_sets : List Set
    , current_set : Int
    }


type alias Set =
    List Param


type alias Param =
    { name : String
    , description : String
    , type_ : String
    , mandatory : Bool
    }


setPos : Int -> Model -> Model
setPos pos model =
    let
        maxPos =
            (List.length model.non_empty_sets) - 1

        newPos =
            if pos > maxPos then
                if maxPos - 1 < 0 then
                    0
                else
                    maxPos - 1
            else if pos < 0 then
                0
            else
                pos
    in
        { model
            | current_set = newPos
        }


update_model : Model -> Model
update_model model =
    case model.has_empty_set of
        Nothing ->
            let
                basic =
                    Just (getBasic model)

                advanced =
                    nonEmptySets model.parameter_sets

                has_advanced =
                    Just (not (List.isEmpty advanced))
            in
                { model
                    | has_empty_set = basic
                    , has_non_empty_sets = has_advanced
                    , non_empty_sets = advanced
                }

        _ ->
            model


hasBasic : Model -> Bool
hasBasic service =
    case service.has_empty_set of
        Nothing ->
            False

        Just bool ->
            bool


hasAdvanced : Model -> Bool
hasAdvanced service =
    case service.has_non_empty_sets of
        Nothing ->
            False

        Just bool ->
            bool


getBasic : Model -> Bool
getBasic service =
    basicAllowed service.parameter_sets


basicAllowed : List Set -> Bool
basicAllowed sets =
    let
        noMandatory param current =
            case param.mandatory of
                True ->
                    False

                False ->
                    current

        hasNoMandatoryFields set current =
            case List.foldl noMandatory True set of
                True ->
                    True

                False ->
                    current
    in
        List.foldl hasNoMandatoryFields False sets


nonEmptySets : List Set -> List Set
nonEmptySets sets =
    let
        nonEmpty set =
            not (List.isEmpty set)
    in
        List.filter nonEmpty sets
