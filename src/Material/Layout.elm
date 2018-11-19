module Material.Layout exposing (Config, Contents, Mode(..), Model, Property, defaultConfig, defaultModel, fixedDrawer, fixedHeader, fixedTabs, moreTabs, onSelectTab, render, rippleTabs, scrolling, seamed, selectedTab, transparentHeader, waterfall)

import Element exposing (..)
import Element.Events as Events
import Material.Options as Options
import Material.Options.Internal as Internal



-- MODEL


type alias Model =
    {}


defaultModel : Model
defaultModel =
    {}



-- PROPERTIES


type Mode
    = Standard
    | Seamed
    | Scrolling
    | Waterfall Bool


type alias Config msg =
    { fixedHeader : Bool
    , fixedDrawer : Bool
    , fixedTabs : Bool
    , rippleTabs : Bool
    , mode : Mode
    , selectedTab : Int
    , onSelectTab : Maybe (Int -> Attribute msg)
    , transparentHeader : Bool
    , moreTabs : Bool
    }


defaultConfig : Config msg
defaultConfig =
    { fixedHeader = False
    , fixedDrawer = False
    , fixedTabs = False
    , rippleTabs = True
    , mode = Standard
    , onSelectTab = Nothing
    , selectedTab = -1
    , moreTabs = False
    , transparentHeader = False
    }


type alias Property msg =
    Options.Property (Config msg) msg


fixedHeader : Property msg
fixedHeader =
    Internal.option (\config -> { config | fixedHeader = True })


fixedDrawer : Property msg
fixedDrawer =
    Internal.option (\config -> { config | fixedDrawer = True })


fixedTabs : Property msg
fixedTabs =
    Internal.option (\config -> { config | fixedTabs = True })


rippleTabs : Property msg
rippleTabs =
    Internal.option (\config -> { config | rippleTabs = True })


waterfall : Bool -> Property msg
waterfall =
    Internal.option
        << (\b config -> { config | mode = Waterfall b })


seamed : Property msg
seamed =
    Internal.option (\config -> { config | mode = Seamed })


transparentHeader : Property msg
transparentHeader =
    Internal.option (\config -> { config | transparentHeader = True })


scrolling : Property msg
scrolling =
    Internal.option (\config -> { config | mode = Scrolling })


selectedTab : Int -> Property msg
selectedTab =
    Internal.option
        << (\k config -> { config | selectedTab = k })


moreTabs : Property msg
moreTabs =
    Internal.option (\config -> { config | moreTabs = True })


onSelectTab : (Int -> msg) -> Property msg
onSelectTab =
    Internal.option << (\f config -> { config | onSelectTab = Just (f >> Events.onClick) })



-- VIEW


type alias Contents m =
    { header : List (Element m)
    , drawer : List (Element m)
    , tabs : List (Element m)
    , main : List (Element m)
    }


render : { model | layout : Model } -> List (Property msg) -> Contents msg -> Element msg
render model properties contents =
    el [] (text "Layout")
