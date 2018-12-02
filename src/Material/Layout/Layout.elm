module Material.Layout.Layout exposing (Config, Contents, Header, Mode(..), Model, Msg, Property, clippedDrawer, defaultConfig, defaultModel, fixedDrawer, fixedHeader, fixedTabs, moreTabs, onSelectTab, rippleTabs, scrolling, seamed, selectedTab, smallScreen, transparentHeader, update, view, waterfall)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html
import Html.Attributes
import Html.Events.Extra.Touch as Touch
import Material.Icon as Icon
import Material.Layout.Drawer as Drawer
import Material.Options as Options exposing (css)
import Material.Options.Internal as Internal



-- MODEL


type alias Model =
    { drawer : Drawer.Model
    , touchCoordinatesStart : Maybe ( Float, Float )
    }


defaultModel : Model
defaultModel =
    { drawer = Drawer.defaultModel
    , touchCoordinatesStart = Nothing
    }



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



-- MESSAGES


type Msg
    = Noop
    | DrawerToggle
    | DrawerTouchStartAt ( Float, Float )
    | DrawerTouchEndAt ( Float, Float )
    | DrawerItemMouseEnter Int
    | DrawerItemMouseLeave Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        drawerModel =
            model.drawer
    in
    case message of
        Noop ->
            ( model, Cmd.none )

        DrawerToggle ->
            ( { model | drawer = { drawerModel | isOpen = not drawerModel.isOpen } }, Cmd.none )

        DrawerTouchStartAt coordinates ->
            case model.touchCoordinatesStart of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | touchCoordinatesStart = Just coordinates }, Cmd.none )

        DrawerTouchEndAt ( endX, endY ) ->
            case model.touchCoordinatesStart of
                Just ( startX, startY ) ->
                    let
                        vectorX =
                            endX - startX

                        vectorY =
                            endY - startY

                        swipeLeft =
                            if abs vectorY < abs vectorX then
                                if vectorX < 0 then
                                    Just True

                                else
                                    Just False

                            else
                                Nothing

                        isDrawerOpen =
                            case swipeLeft of
                                Just True ->
                                    False

                                _ ->
                                    drawerModel.isOpen
                    in
                    ( { model | touchCoordinatesStart = Nothing, drawer = { drawerModel | isOpen = isDrawerOpen } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DrawerItemMouseEnter itemIndex ->
            ( { model | drawer = { drawerModel | hovered = itemIndex } }, Cmd.none )

        DrawerItemMouseLeave itemIndex ->
            if model.drawer.hovered == itemIndex then
                ( { model | drawer = { drawerModel | hovered = -1 } }, Cmd.none )

            else
                ( model, Cmd.none )



-- VIEW


type alias Header msg =
    { title : Element msg
    , actionButtons : List (Element msg)
    }


type alias Contents msg =
    { header : Header msg
    , drawer : List (Drawer.DrawerElement msg)
    , tabs : List (Element msg)
    , main : List (Element msg)
    }


view : (Msg -> msg) -> Model -> List (Property msg) -> Contents msg -> Element msg
view lift model properties { header, drawer, tabs, main } =
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

        drawerIsVisible =
            model.drawer.isOpen && not drawerIsFixed

        headerDrawerButton =
            if hasDrawer && not drawerIsFixed then
                Just (drawerButton lift)

            else
                Nothing

        headerElement =
            headerView config model ( headerDrawerButton, header )

        drawerElement =
            Drawer.view
                (if not drawerIsFixed then
                    [ htmlAttribute <| Html.Attributes.style "box-shadow" "0 8px 10px -5px rgba(0,0,0,.2), 0 16px 24px 2px rgba(0,0,0,.14), 0 6px 30px 5px rgba(0,0,0,.12)"
                    , htmlAttribute <|
                        Html.Attributes.style "transform"
                            (if drawerIsVisible then
                                "none"

                             else
                                "translateX(calc(-100% - 20px))"
                            )
                    , htmlAttribute <| Html.Attributes.style "will-change" "transform"
                    , htmlAttribute <| Html.Attributes.style "transition" "all .25s"
                    , htmlAttribute <| Touch.onWithOptions "touchstart" { stopPropagation = False, preventDefault = False } (lift << DrawerTouchStartAt << touchCoordinates)
                    , htmlAttribute <| Touch.onWithOptions "touchend" { stopPropagation = False, preventDefault = False } (lift << DrawerTouchEndAt << touchCoordinates)
                    ]

                 else
                    []
                )
                model.drawer
                { elements = drawer, itemEvents = { onMouseEnter = lift << DrawerItemMouseEnter, onMouseLeave = lift << DrawerItemMouseLeave } }

        scrimElement =
            if drawerIsFixed then
                Nothing

            else
                Just
                    (scrim lift
                        ((htmlAttribute <| Html.Attributes.style "will-change" "opacity")
                            :: (htmlAttribute <| Html.Attributes.style "transition" "all .25s")
                            :: (if drawerIsVisible then
                                    [ Events.onClick (lift DrawerToggle) ]

                                else
                                    [ htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                                    , htmlAttribute <| Html.Attributes.style "opacity" "0"
                                    ]
                               )
                        )
                    )
    in
    if drawerIsFixed then
        if drawerIsClipped then
            row
                [ width fill
                , height fill
                , inFront headerElement
                ]
                [ row
                    [ height fill
                    , paddingEach { top = 56, bottom = 0, left = 0, right = 0 }
                    ]
                    [ drawerElement ]
                ]

        else
            row
                [ width fill
                , height fill
                ]
                [ drawerElement
                , el [ width fill, height fill, inFront headerElement ] (text "Layout")
                ]

    else
        el
            [ width fill
            , height fill
            , inFront headerElement
            , inFront (Maybe.withDefault none scrimElement)
            , inFront drawerElement
            ]
            (text "Layout")


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


headerView : Config msg -> Model -> ( Maybe (Element msg), Header msg ) -> Element msg
headerView config model ( headerDrawerButton, header ) =
    let
        leftPadding =
            case headerDrawerButton of
                Just _ ->
                    0

                Nothing ->
                    8
    in
    row
        [ width fill
        , height (px 56)
        , Background.color (rgb 0 0 0)
        , Font.color (rgb 255 255 255)
        , paddingEach { top = 16, bottom = 16, left = leftPadding, right = 16 }
        , htmlAttribute <| Html.Attributes.style "box-shadow" "0 2px 4px -1px rgba(0,0,0,.2), 0 4px 5px 0 rgba(0,0,0,.14), 0 1px 10px 0 rgba(0,0,0,.12)"
        ]
        [ Maybe.withDefault none headerDrawerButton
        , el [ paddingXY 16 0 ] header.title
        , row [ alignRight, spacing 24 ] (List.map (el []) header.actionButtons)
        ]


drawerButton : (Msg -> msg) -> Element msg
drawerButton lift =
    el
        [ padding 16
        , pointer
        , htmlAttribute <| Html.Attributes.type_ "button"
        , Events.onClick (lift DrawerToggle)
        ]
        (Icon.button24 "menu" [] [])


scrim : (Msg -> msg) -> List (Attribute msg) -> Element msg
scrim lift attributes =
    el
        ([ width fill
         , height fill
         , Background.color (rgba 0 0 0 0.32)
         ]
            ++ attributes
        )
        none
