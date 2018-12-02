module Material.Options exposing (Property, cs, css, none, styled)

import Element exposing (Attribute)
import Material.Options.Internal as Internal exposing (..)


type alias Property config msg =
    Internal.Property config msg


none : Property config msg
none =
    None


cs : String -> Property config msg
cs name =
    Class name


css : String -> String -> Property config msg
css key value =
    CSS ( key, value )


styled : (List (Attribute msg) -> a) -> List (Property config msg) -> a
styled element props =
    element
        (addAttributes
            (collect_ props)
            []
        )
