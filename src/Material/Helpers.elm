module Material.Helpers exposing (lift)


type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


type alias Update model action =
    Update_ model action action


lift :
    (model -> submodel)
    ->
        -- get
        (model -> submodel -> model)
    ->
        -- set
        (subaction -> action)
    ->
        -- fwd
        Update submodel subaction
    ->
        -- update
        subaction
    ->
        -- action
        model
    ->
        -- model
        ( model, Cmd action )
lift get set fwd update action model =
    let
        ( innerModel, e ) =
            update action (get model)
    in
    ( set model innerModel, Cmd.map fwd e )
