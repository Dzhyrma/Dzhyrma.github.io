module Material.Layout exposing (Config, Contents, Header, Mode(..), Model, Property, defaultConfig, defaultModel, fixedDrawer, fixedHeader, fixedTabs, moreTabs, onSelectTab, rippleTabs, scrolling, seamed, selectedTab, smallScreen, transparentHeader, view, waterfall)

import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html.Attributes
import Material.Icon as Icon
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
    { smallScreen : Bool
    , fixedHeader : Bool
    , fixedDrawer : Bool
    , clippedDrawer : Bool
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
    { smallScreen = False
    , fixedHeader = False
    , fixedDrawer = False
    , clippedDrawer = False
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


smallScreen : Property msg
smallScreen =
    Internal.option (\config -> { config | smallScreen = True })


fixedHeader : Property msg
fixedHeader =
    Internal.option (\config -> { config | fixedHeader = True })


fixedDrawer : Property msg
fixedDrawer =
    Internal.option (\config -> { config | fixedDrawer = True })


clippedDrawer : Property msg
clippedDrawer =
    Internal.option (\config -> { config | clippedDrawer = True })


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
        << (\tabIndex config -> { config | selectedTab = tabIndex })


moreTabs : Property msg
moreTabs =
    Internal.option (\config -> { config | moreTabs = True })


onSelectTab : (Int -> msg) -> Property msg
onSelectTab =
    Internal.option << (\f config -> { config | onSelectTab = Just (f >> Events.onClick) })



-- VIEW


type alias Header msg =
    { title : Element msg
    , actionButtons : List (Element msg)
    }


type alias Contents msg =
    { header : Header msg
    , drawer : List (Element msg)
    , tabs : List (Element msg)
    , main : List (Element msg)
    }


view : Model -> List (Property msg) -> Contents msg -> Element msg
view model properties { header, drawer, tabs, main } =
    let
        summary =
            Internal.collect defaultConfig properties

        config =
            summary.config

        hasDrawer =
            drawer /= []

        drawerIsFixed =
            config.fixedDrawer && not config.smallScreen

        drawerIsClipped =
            config.clippedDrawer

        headerDrawerButton =
            if hasDrawer then
                Just drawerButton

            else
                Nothing
    in
    el
        [ width fill
        , height fill
        , inFront (headerView config model ( headerDrawerButton, header ))
        , inFront (drawerView config model drawer)
        ]
        (text "Layout")


headerView : Config msg -> Model -> ( Maybe (Element msg), Header msg ) -> Element msg
headerView config model ( headerDrawerButton, header ) =
    row
        [ width fill
        , height (px 56)
        , Background.color (rgb 0 0 0)
        , Font.color (rgb 255 255 255)
        , padding 16
        , htmlAttribute <| Html.Attributes.style "box-shadow" "0 2px 4px -1px rgba(0,0,0,.2), 0 4px 5px 0 rgba(0,0,0,.14), 0 1px 10px 0 rgba(0,0,0,.12)"
        ]
        [ drawerButton
        , el [ paddingXY 32 0 ] header.title
        , row [ alignRight, spacing 24 ] (List.map (el []) header.actionButtons)
        ]


drawerButton : Element msg
drawerButton =
    el [] (Icon.i "menu")


drawerView : Config msg -> Model -> List (Element msg) -> Element msg
drawerView config model elements =
    column
        [ width (px 256)
        , height fill
        , Background.color (rgb 255 255 255)
        , padding 16
        ]
        [ text "drawer" ]
