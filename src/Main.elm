module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Material.Helpers exposing (lift)
import Material.Icon as Icon
import Material.Layout as Layout
import Task exposing (..)
import Url exposing (Url)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Flags =
    WindowSize


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url
    , windowSize : WindowSize
    , layout : Layout.Model
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , windowSize = flags
      , layout = Layout.defaultModel
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeUrl String
    | WindowSizeChanged Int Int
    | LayoutMsg Layout.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        ChangeUrl urlPath ->
            ( model, Nav.pushUrl model.key urlPath )

        WindowSizeChanged newWidth newHeight ->
            ( { model | windowSize = WindowSize newWidth newHeight }, Cmd.none )

        LayoutMsg a ->
            lift .layout (\m x -> { m | layout = x }) LayoutMsg Layout.update a model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowSizeChanged



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Andrii Dzhyrma"
    , body =
        [ body model
        ]
    }


body : Model -> Html Msg
body model =
    let
        device =
            classifyDevice model.windowSize
    in
    Element.layout []
        (case device.class of
            BigDesktop ->
                Layout.view LayoutMsg
                    model.layout
                    [ Layout.fixedDrawer
                    , Layout.clippedDrawer
                    ]
                    { header = header model
                    , drawer = [ text "Drawer element" ]
                    , tabs = []
                    , main = []
                    }

            Desktop ->
                Layout.view LayoutMsg
                    model.layout
                    [ Layout.fixedDrawer
                    ]
                    { header = header model
                    , drawer = [ text "Drawer element" ]
                    , tabs = []
                    , main = []
                    }

            Tablet ->
                Layout.view LayoutMsg
                    model.layout
                    [ Layout.smallScreen
                    ]
                    { header = header model
                    , drawer = [ text "Drawer element" ]
                    , tabs = []
                    , main = []
                    }

            Phone ->
                Layout.view LayoutMsg
                    model.layout
                    [ Layout.smallScreen
                    ]
                    { header = header model
                    , drawer = [ text "Drawer element" ]
                    , tabs = []
                    , main = []
                    }
        )


header : Model -> Layout.Header msg
header model =
    Layout.Header (text "Main page") [ text "About me", Icon.i "search", Icon.i "more_vert" ]
