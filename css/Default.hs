{-# LANGUAGE OverloadedStrings #-}

module Default (defaultCss) where
import Prelude hiding (rem)

import Clay
import Clay.Media (maxWidth, minWidth)

defaultCss :: Css
defaultCss = do
    html ? do
        fontSize (pct 62.5)
        fontFamily ["Source Sans Pro", "Noto Sans TC"] [sansSerif]

    body ? do
        fontSize (rem 1.6)
        color black
        backgroundColor "#F5F5F5"

    code ? do
        padding (px 0) (px 0.2) (px 0) (px 0)
        fontFamily ["Roboto Mono"] [monospace]

    header ? borderBottom (rem 0.2) solid black

    nav ? textAlign (alignSide sideRight)

    nav |> a ? do
        fontSize (rem 1.8)
        fontWeight bold
        color black
        textDecoration none
        textTransform uppercase

    ".contacts" |> a ? do
        display inlineBlock
        marginTop (rem 2.4)
        padding (px 0.2) (px 0.2) (px 0.2) (px 0.2)
        fontSize (rem 2.4)
        color "#555"
        textDecoration none

    footer ? do
        borderTop (px 0.2) solid black
        padding (rem 1.2) (px 0) (px 0) (px 0)
        fontSize (rem 1.2)
        color "#555"

    img ? do
        display block
        marginLeft auto
        marginRight auto

    h1 ? fontSize (rem 2.4)

    h2 ? do
        borderTop (px 1) solid "#E6E6E6"
        paddingTop (px 20)
        fontSize (rem 2)

    h3 ? do
        textAlign (alignSide sideLeft)

    "article .header" ? do
        fontSize (rem 1.4)
        fontStyle italic
        color "#555"

    ".logo" |> a ? do
        fontWeight bold
        color black
        textDecoration none

    query Clay.all [Clay.Media.maxWidth (px 359)] $ do
        body ? do
            width $ pct 90
            margin auto auto auto auto
            paddingTop $ px 0
            paddingRight $ pct 5
            paddingBottom $ px 0
            paddingLeft $ px 0
        header ? margin (rem 4.2) (px 0) (px 0) (px 0)
        nav ? do
            margin (px 0) auto (rem 3) (px 0)
            textAlign center
        footer ? textAlign center
        ".logo" ? do
            textAlign center
            margin (rem 1) auto (rem 3) (px 0)
        ".logo" |> a ? fontSize (rem 2.4)
        nav |> a ? do
            display block
            lineHeight (rem 1.6)
        img ? do
            border (px 20) solid none
            width (px 200)

    query Clay.all [Clay.Media.minWidth (px 360)] $ do
        body ? do
            width $ pct 90
            margin auto auto auto auto
            paddingTop $ px 0
            paddingRight $ pct 5
            paddingBottom $ px 0
            paddingLeft $ px 0
        header ? margin (rem 4.2) (px 0) (px 0) (px 0)
        nav ? do
            margin (px 0) auto (rem 3) (px 0)
            textAlign center
        footer ? textAlign center
        ".logo" ? do
            textAlign center
            margin (rem 1) auto (rem 3) (px 0)
        ".logo" |> a ? fontSize (rem 2.4)
        nav |> a ? do
            display inline
            margin (px 0) (rem 0.6) (px 0) (px 0)
        img ? do
            border (px 40) solid none
            width (px 320)

    query Clay.all [Clay.Media.minWidth (px 640)] $ do
        body ? do
            width (pct 60)
            margin auto auto auto auto
            padding (px 0) (px 0) (px 0) (px 0)
        header ? do
            margin (px 0) (px 0) (rem 3) (px 0)
            padding (rem 1.2) (px 0) (rem 1.2) (px 0)
        nav ? do
            margin (px 0) (px 0) (px 0) (px 0)
            textAlign (alignSide sideRight)
        nav |> a ? do
            margin (px 0) (px 0) (px 0) (rem 1.2)
            display inline
        footer ? textAlign (alignSide sideRight)
        img ? do
            border (px 40) solid none
            width (px 320)
        "figcaption" ? do
            textAlign center
            marginTop (px 18)
        ".logo" ? do
            margin (px 0) (px 0) (px 0) (px 0)
            textAlign (alignSide sideLeft)
        ".logo" |> a ? do
            float floatLeft
            fontSize (rem 1.8)
        ".comments" ? do
            paddingTop (px 5)
            borderTop (px 2) solid black
            marginTop (px 60)

main :: IO ()
main = putCss defaultCss