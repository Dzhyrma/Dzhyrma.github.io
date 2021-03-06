module Material.Icon exposing (Config, Property, button24, defaultConfig, i, size18, size24, size36, size48, view)

import Element exposing (..)
import Html
import Html.Attributes
import Material.Options as Options exposing (Property, cs, css)
import Material.Options.Internal as Internal exposing (collect_)



-- PROPERTIES


type alias Config =
    {}


type alias Property msg =
    Options.Property Config msg


defaultConfig : Config
defaultConfig =
    {}


size18 : Property msg
size18 =
    css "font-size" "18px"


size24 : Property msg
size24 =
    css "font-size" "24px"


size36 : Property msg
size36 =
    css "font-size" "36px"


size48 : Property msg
size48 =
    css "font-size" "48px"



-- VIEW


button24 : String -> List (Property msg) -> List (Property msg) -> Element msg
button24 name buttonOptions iconOptions =
    let
        allButtonOptions =
            css "background" "none"
                :: css "border" "0"
                :: css "outline" "none"
                :: css "cursor" "pointer"
                :: css "padding" "0"
                :: css "font-family" "inherit"
                :: css "color" "inherit"
                :: css "font" "inherit"
                :: css "height" "24px"
                :: css "width" "24px"
                :: buttonOptions
    in
    (styled Html.button allButtonOptions <| [ styled Html.i (cs "material-icons" :: iconOptions) <| [ Html.text name ] ]) |> html


view : String -> List (Property msg) -> Element msg
view name options =
    let
        iOptions =
            cs "material-icons"
                :: options
    in
    (styled Html.i iOptions <| [ Html.text name ]) |> html


styled : (List (Html.Attribute msg) -> a) -> List (Property msg) -> a
styled ctor options =
    let
        summary =
            collect_ options

        htmlAttributes =
            List.map (\( key, value ) -> Html.Attributes.style key value) summary.css
                ++ [ Html.Attributes.class (String.join " " summary.classes) ]
    in
    ctor htmlAttributes


i : String -> Element msg
i name =
    view name []
