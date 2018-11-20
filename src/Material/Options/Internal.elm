module Material.Options.Internal exposing (Property(..), Summary, addAttributes, attribute, collect, collect1, collect1_, collect_, option, recollect)

import Element exposing (..)
import Html.Attributes exposing (style)


type Property config msg
    = Class String
    | CSS ( String, String )
    | Internal (Attribute msg)
    | Set (config -> config)
    | None


attribute : Attribute msg -> Property config msg
attribute =
    Internal


option : (config -> config) -> Property config msg
option =
    Set


type alias Summary config msg =
    { classes : List String
    , css : List ( String, String )
    , attrs : List (Attribute msg)
    , internal : List (Attribute msg)
    , config : config
    }


collect1 : Property config msg -> Summary config msg -> Summary config msg
collect1 property summary =
    case property of
        Class entry ->
            { summary | classes = entry :: summary.classes }

        CSS entry ->
            { summary | css = entry :: summary.css }

        Internal attr ->
            { summary | internal = attr :: summary.internal }

        Set setter ->
            { summary | config = setter summary.config }

        None ->
            summary


collect1_ : Property config msg -> Summary () msg -> Summary () msg
collect1_ property summary =
    case property of
        Class entry ->
            { summary | classes = entry :: summary.classes }

        CSS entry ->
            { summary | css = entry :: summary.css }

        Internal attr ->
            { summary | internal = attr :: summary.internal }

        Set setter ->
            summary

        None ->
            summary


recollect : Summary config msg -> List (Property config msg) -> Summary config msg
recollect =
    List.foldl collect1


collect : config -> List (Property config msg) -> Summary config msg
collect =
    Summary [] [] [] [] >> recollect


collect_ : List (Property c m) -> Summary () m
collect_ =
    List.foldl collect1_ (Summary [] [] [] [] ())


addAttributes : Summary config msg -> List (Attribute msg) -> List (Attribute msg)
addAttributes summary attrs =
    summary.attrs
        ++ (List.map htmlAttribute <| List.map (\( key, value ) -> Html.Attributes.style key value) summary.css)
        ++ [ htmlAttribute <| Html.Attributes.class (String.join " " summary.classes) ]
        ++ attrs
        ++ summary.internal
