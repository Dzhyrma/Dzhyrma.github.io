module Material.Layout.Drawer exposing (divider, item, title, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- VIEW


view : List (Attribute msg) -> List (Element msg) -> Element msg
view attributes elements =
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
        elements


title : String -> Maybe String -> Element msg
title titleText subtitleText =
    column
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


item : List (Attribute msg) -> String -> Element msg
item attributes itemText =
    row
        ([ width fill
         , height (px 48)
         , paddingXY 16 0
         , Font.color (rgba255 0 0 0 0.87)
         , Font.size 16
         ]
            ++ attributes
        )
        [ el [ centerY ] (text itemText) ]


divider : Element msg
divider =
    el
        [ width fill
        , height (px 1)
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color (rgba255 0 0 0 0.12)
        , paddingEach { bottom = 0, top = 8, left = 0, right = 0 }
        ]
        none
