module Config where

import Color
import Time

w = 500
h = 500

buttonsHeight = 200

totalHeight = h + buttonsHeight

transitionTime = Time.second

colors : List Color.Color
colors =
  [ Color.rgb 0 119 219
  , Color.rgb 204 0 51
  ]
