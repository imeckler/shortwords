module Style where

import Util(..)
import Color
import String
import Html
import Config(..)

backgroundColor       = Color.rgb 223 223 223
borderColor           = Color.rgb 188 188 188
fadeColor             = Color.rgb 254 204 9
winTextColor          = Color.rgb 75 91 110
buttonBackgroundColor = winTextColor
rotateArcColor        = fadeColor
movesLeftTextColor    = winTextColor
movesLeftCircleColor  = fadeColor
defTextStyle h        =
  { typeface = ["Josefin Sans", "sans-serif"]
  , height   = Just h
  , color    = winTextColor
  , bold     = False
  , italic   = False
  , line     = Nothing
  }
defaultFontStr = String.join "," ((defTextStyle 0).typeface)

frameWidth = w + 8
frameHeight = totalHeight + 4

globalStyle = 
  let buttonColor = Color.rgb 0 119 219 in
  Html.toElement 0 0 <| styleNode <|
  [ ( ".win-difficulty-button"
    , [ ("pointer-events", "auto") ]
    )
  , ( ".swbutton"
    , [ ("pointer-events", "auto")
      , ("width", px customButtonW)
      , ("height", px customButtonH)
      , ("line-height", px customButtonH)
      , ("background-color", colorStr buttonColor)
      , ("text-align", "center")
      , ("border-radius", px 9)
      , ("border", "3px solid black")
      , ("margin", "0 auto")
      , ("font-size", px 20)
      , ("color", colorStr Color.white)
      , ("cursor", "pointer")
      ]
    )
  , ( ".swbutton:hover"
    , [ ("background-color", colorStr (lighten buttonColor)) ]
    )
  , ( ".swbutton:active"
    , [ ("background-color", colorStr (darken buttonColor)) ]
    )
  , ( ".top-isom"
    , [ ("border-top-left-radius", px 8)
      , ("border-top-right-radius", px 8)
      , ("border-top", "4px solid black")
      ]
    )
  , ( ".bottom-isom"
    , [ ("border-bottom-left-radius", px 8)
      , ("border-bottom-right-radius", px 8)
      , ("border-bottom", "4px solid black")
      ]
    )
  , ( ".isom"
    , [ ("border-left", "4px solid black")
      , ("border-right", "4px solid black")
      , ("cursor", "pointer")
      ]
    )
  , ( ".movebutton"
    , [ ("display", "inline-block")
      , ("margin", px 7)
      , ("pointer-events", "auto")
      ]
    )
  , ( ".movebutton:hover"
    , [("opacity", toString 0.6)]
    )
  , ( "#mainframe"
    , [ ("-webkit-box-sizing", "border-box")
      , ("-moz-box-sizing", "border-box")
      , ("box-sizing", "border-box")
      , ("width", px frameWidth)
      , ("height", px frameHeight)
      , ("border", "4px solid black")
      , ("border-radius", px 8)
      ]
    )
  , ( "#explanationtext"
    , [ ("width", px (w - 60))
      , ("margin-left", "auto")
      , ("margin-right", "auto")
      , ("text-align", "left")
      , ("color", colorStr (defTextStyle 0).color)
      ]
    )
  , ( "#titletext"
    , [ ("font-size", px 70)
      , ("text-align", "center")
      , ("margin", "0 auto")
      , ("font-weight", "800")
      , ("color", colorStr (defTextStyle 0).color)
      ]
    )
  , ( ".level-button"
    , [ ("width", px levelButtonH), ("height", px levelButtonW)
      , ("line-height", px levelButtonH)
      , ("margin", "0")
      , ("pointer-events", "auto")
      , ("text-align", "center")
      ]
    )
  , ( ".level-button.active"
    , [ ("color", colorStr Color.black)
      , ("cursor", "pointer")
      ]
    )
  , ( ".level-button.inactive"
    , [ ("color", colorStr (Color.grayscale 0.2))
      , ("cursor", "pointer")
      ]
    )
  , ( ".level-button.active:hover"
    , [ ("color", colorStr (Color.grayscale 0.1))
      , ("background-color", colorStr (Color.grayscale 0.5))
      ]
    )
  , ( ".level-button.active:active"
    , [ ("color", colorStr Color.white)
      , ("background-color", colorStr Color.black)
      ]
    )
  , ( ".level-button.current"
    , [ ("color", colorStr Color.white)
      , ("background-color", colorStr Color.black)
      , ("cursor", "default")
      ]
    )
  , ("body", [("font-family", defaultFontStr)])
  ]

levelButtonH = 40
levelButtonW = 40

customButtonW = 150
customButtonH = 50

lighten c =
  let {hue,saturation,lightness,alpha} = Color.toHsl c in
  Color.hsla hue saturation (2*lightness) 1

darken c =
  let {hue,saturation,lightness,alpha} = Color.toHsl c in
  Color.hsla hue saturation (0.5*lightness) 1

