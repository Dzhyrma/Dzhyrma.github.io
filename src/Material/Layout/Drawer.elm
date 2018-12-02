module Material.Layout.Drawer exposing (Contents, DrawerElement, Model, defaultModel, divider, item, title, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes



-- MODEL


type alias Model =
    { isOpen : Bool
    , hovered : Int
    }


defaultModel : Model
defaultModel =
    { isOpen = False
    , hovered = -1
    }



-- PROPERTIES


type DrawerElement msg
    = Clickable (List (Attribute msg) -> Element msg)
    | Static (Element msg)


type alias ItemEvents msg =
    { onMouseEnter : Int -> msg
    , onMouseLeave : Int -> msg
    }


type alias Contents msg =
    { elements : List (DrawerElement msg)
    , itemEvents : ItemEvents msg
    }



-- VIEW


view : List (Attribute msg) -> Model -> Contents msg -> Element msg
view attributes model { elements, itemEvents } =
    column
        ([ width (px 256)
         , height fill
         , Background.color (rgb255 255 255 255)
         , Border.widthEach { bottom = 0, top = 0, left = 0, right = 1 }
         , Border.color (rgba255 0 0 0 0.12)
         , scrollbarY
         ]
            ++ attributes
        )
        (transform_ itemEvents model elements)


transform_ : ItemEvents msg -> Model -> List (DrawerElement msg) -> List (Element msg)
transform_ { onMouseEnter, onMouseLeave } model elements =
    List.indexedMap
        (\i drawerElement ->
            case drawerElement of
                Clickable element ->
                    element
                        [ Events.onMouseEnter (onMouseEnter i)
                        , Events.onMouseLeave (onMouseLeave i)
                        , if i == model.hovered then
                            Background.color (rgba 0 0 0 0.04)

                          else
                            Background.color (rgba 0 0 0 0)
                        ]

                Static element ->
                    element
        )
        elements


title : String -> Maybe String -> DrawerElement msg
title titleText subtitleText =
    Static
        (column
            [ width fill
            , height (px 64)
            , paddingXY 16 0
            , spacing 6
            ]
            [ el [ centerY ] (text titleText)
            , case subtitleText of
                Just subtitle ->
                    el
                        [ centerY
                        , Font.color (rgba255 0 0 0 0.54)
                        , Font.size 13
                        ]
                        (text subtitle)

                Nothing ->
                    none
            ]
        )


item : List (Attribute msg) -> String -> DrawerElement msg
item attributes itemText =
    Clickable (item_ attributes itemText)


item_ : List (Attribute msg) -> String -> List (Attribute msg) -> Element msg
item_ attributes itemText drawerAttributes =
    row
        ([ width fill
         , height (px 48)
         , paddingXY 16 0
         , Font.color (rgba255 0 0 0 0.87)
         , Font.size 16
         ]
            ++ attributes
            ++ drawerAttributes
        )
        [ el [ centerY ] (text itemText) ]


divider : DrawerElement msg
divider =
    Static
        (el
            [ width fill
            , height (px 1)
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color (rgba255 0 0 0 0.12)
            , paddingEach { bottom = 0, top = 8, left = 0, right = 0 }
            ]
            none
        )
