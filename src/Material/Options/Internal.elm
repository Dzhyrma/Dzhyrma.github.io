module Material.Options.Internal exposing (Property(..), attribute, option)

import Element exposing (..)


type Property config msg
    = Internal (Attribute msg)
    | Set (config -> config)


attribute : Attribute msg -> Property config msg
attribute =
    Internal


option : (config -> config) -> Property config msg
option =
    Set
