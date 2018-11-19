module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
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
    , url : Url.Url
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


body : Model -> Html msg
body model =
    let
        device =
            classifyDevice model.windowSize
    in
    Element.layout []
        (case device.class of
            BigDesktop ->
                Layout.render model
                    [ Layout.fixedDrawer
                    ]
                    { header = []
                    , drawer = []
                    , tabs = []
                    , main = []
                    }

            Desktop ->
                text "Desktop"

            Tablet ->
                text "Tablet"

            Phone ->
                text "Phone"
        )
