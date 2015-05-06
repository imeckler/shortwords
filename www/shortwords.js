var Elm = Elm || { Native: {} };
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,empty: empty
                       ,repeat: repeat
                       ,initialize: initialize
                       ,fromList: fromList
                       ,length: length
                       ,push: push
                       ,append: append
                       ,get: get
                       ,set: set
                       ,slice: slice
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,filter: filter
                       ,foldl: foldl
                       ,foldr: foldr};
   return _elm.Array.values;
};
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 595, column 3 to 8");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 573, column 3 to 4");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 567, column 3 to 4");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Char",
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,
   high,
   $char) {
      return function () {
         var code = toCode($char);
         return _U.cmp(code,
         toCode(low)) > -1 && _U.cmp(code,
         toCode(high)) < 1;
      }();
   });
   var isUpper = A2(isBetween,
   _U.chr("A"),
   _U.chr("Z"));
   var isLower = A2(isBetween,
   _U.chr("a"),
   _U.chr("z"));
   var isDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("9"));
   var isOctDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,
      _U.chr("A"),
      _U.chr("F"),
      $char));
   };
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 150 and 152"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 114 and 121");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 111");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgb: rgb
                       ,rgba: rgba
                       ,hsl: hsl
                       ,hsla: hsla
                       ,greyscale: greyscale
                       ,grayscale: grayscale
                       ,complement: complement
                       ,linear: linear
                       ,radial: radial
                       ,toRgb: toRgb
                       ,toHsl: toHsl
                       ,red: red
                       ,orange: orange
                       ,yellow: yellow
                       ,green: green
                       ,blue: blue
                       ,purple: purple
                       ,brown: brown
                       ,lightRed: lightRed
                       ,lightOrange: lightOrange
                       ,lightYellow: lightYellow
                       ,lightGreen: lightGreen
                       ,lightBlue: lightBlue
                       ,lightPurple: lightPurple
                       ,lightBrown: lightBrown
                       ,darkRed: darkRed
                       ,darkOrange: darkOrange
                       ,darkYellow: darkYellow
                       ,darkGreen: darkGreen
                       ,darkBlue: darkBlue
                       ,darkPurple: darkPurple
                       ,darkBrown: darkBrown
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,black: black
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray};
   return _elm.Color.values;
};
Elm.Config = Elm.Config || {};
Elm.Config.make = function (_elm) {
   "use strict";
   _elm.Config = _elm.Config || {};
   if (_elm.Config.values)
   return _elm.Config.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Config",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Time = Elm.Time.make(_elm);
   var colors = _L.fromArray([A3($Color.rgb,
                             0,
                             119,
                             219)
                             ,A3($Color.rgb,204,0,51)
                             ,A3($Color.rgb,105,45,172)]);
   var maxSheets = 3;
   var transitionTime = $Time.second / 2;
   var buttonsHeight = 300;
   var maxTransLen = 100;
   var h = 500;
   var totalHeight = h + buttonsHeight;
   var w = 500;
   _elm.Config.values = {_op: _op
                        ,w: w
                        ,h: h
                        ,maxTransLen: maxTransLen
                        ,buttonsHeight: buttonsHeight
                        ,totalHeight: totalHeight
                        ,transitionTime: transitionTime
                        ,maxSheets: maxSheets
                        ,colors: colors};
   return _elm.Config.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 408 and 416");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 397 and 405");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 173 and 179");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 129 and 135");
              }();}
         _U.badCase($moduleName,
         "between lines 124 and 135");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 138 and 140");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 100 and 121");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 70 and 72");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 385 and 394");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 56 and 60");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 145 and 157");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 330 and 332");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 369 and 371");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 235 and 239");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 244 and 248");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 253 and 255");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 377 and 382");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 314 and 319");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 273 and 277");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 271 and 300");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 185 and 189");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 192 and 197");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 206 and 211");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 199 and 204");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 190 and 211");
                    }();}
               _U.badCase($moduleName,
               "between lines 183 and 211");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 213 and 219");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 457, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 478 and 480");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.Draw = Elm.Draw || {};
Elm.Draw.make = function (_elm) {
   "use strict";
   _elm.Draw = _elm.Draw || {};
   if (_elm.Draw.values)
   return _elm.Draw.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Draw",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Config = Elm.Config.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Inputs = Elm.Inputs.make(_elm),
   $Isom = Elm.Isom.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Piece$Infix = Elm.Piece.Infix.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Style = Elm.Style.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Time = Elm.Time.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm),
   $Util = Elm.Util.make(_elm);
   var overlayDifficultyButton = function (d) {
      return A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("level-button active")
                   ,A2($Html$Events.onClick,
                   $Inputs.playLevelOfDifficultyChan.address,
                   d)]),
      _L.fromArray([$Html.text($Basics.toString(d))]));
   };
   var difficultyButtonsOverlay = A2($Html.toElement,
   $Style.levelButtonW,
   4 * $Style.levelButtonH)($Html.div(_L.fromArray([$Html$Attributes.id("level-button-panel")]))(A2($List.map,
   overlayDifficultyButton,
   _L.fromArray([$GameTypes.S
                ,$GameTypes.M
                ,$GameTypes.L
                ,$GameTypes.XL]))));
   var resetButton = A2($Html.toElement,
   $Config.w,
   $Style.customButtonH)(A2($Html.div,
   _L.fromArray([$Html$Attributes.$class("swbutton")
                ,A2($Html$Events.onClick,
                $Inputs.resetLevelChan.address,
                {ctor: "_Tuple0"})
                ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                      ,_0: "width"
                                                      ,_1: $Util.px(100)}]))]),
   _L.fromArray([$Html.text("Reset")])));
   var resetButtonForm = $Graphics$Collage.move({ctor: "_Tuple2"
                                                ,_0: $Config.w / 2 - 60
                                                ,_1: (0 - $Config.h) / 2 + 40})($Graphics$Collage.toForm(resetButton));
   var loseAnimDuration = $Config.transitionTime + $Time.second * (1 / 5 + 1 / 4 + 1 / 5 + 1 / 4);
   var socialButtonSize = 40;
   var facebookButton = A2($Html.a,
   _L.fromArray([$Html$Attributes.id("facebook")
                ,$Html$Attributes.$class("iconcon")
                ,$Html$Attributes.target("_blank")
                ,$Html$Attributes.href("https://www.facebook.com/sharer/sharer.php")]),
   _L.fromArray([A2($Html.div,
   _L.fromArray([$Html$Attributes.$class("icon")
                ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                      ,_0: "width"
                                                      ,_1: $Util.px(socialButtonSize)}
                                                     ,{ctor: "_Tuple2"
                                                      ,_0: "height"
                                                      ,_1: $Util.px(socialButtonSize)}]))]),
   _L.fromArray([]))]));
   var tweetButton = function () {
      var spellOut = function (difficulty) {
         return function () {
            switch (difficulty.ctor)
            {case "L": return "large";
               case "M": return "medium";
               case "S": return "small";
               case "XL":
               return "extra large";}
            _U.badCase($moduleName,
            "between lines 331 and 336");
         }();
      };
      var article = function (difficulty) {
         return function () {
            switch (difficulty.ctor)
            {case "XL": return "an";}
            return "a";
         }();
      };
      return F2(function (difficulty,
      score) {
         return A2($Html.a,
         _L.fromArray([$Html$Attributes.id("tweet")
                      ,$Html$Attributes.$class("iconcon")
                      ,$Html$Attributes.target("_blank")
                      ,$Html$Attributes.href(A2($Basics._op["++"],
                      "https://twitter.com/intent/tweet?text=I+just+completed+",
                      A2($Basics._op["++"],
                      article(difficulty),
                      A2($Basics._op["++"],
                      "+",
                      A2($Basics._op["++"],
                      spellOut(difficulty),
                      A2($Basics._op["++"],
                      "+puzzle+in+%23ShortWords+and+my+score+is+",
                      A2($Basics._op["++"],
                      $Basics.toString(score),
                      ".+http%3A%2F%2Fbit.ly%2F1BWGPNt")))))))]),
         _L.fromArray([A2($Html.div,
         _L.fromArray([$Html$Attributes.$class("icon")
                      ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                            ,_0: "width"
                                                            ,_1: $Util.px(socialButtonSize)}
                                                           ,{ctor: "_Tuple2"
                                                            ,_0: "height"
                                                            ,_1: $Util.px(socialButtonSize)}]))]),
         _L.fromArray([]))]));
      });
   }();
   var diffButtonH = 100;
   var diffButtonW = 100;
   var difficultyButton = function (difficulty) {
      return A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("win-difficulty-button")
                   ,A2($Html$Events.onClick,
                   $Inputs.playLevelOfDifficultyChan.address,
                   difficulty)
                   ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "width"
                                                         ,_1: $Util.px(diffButtonW)}
                                                        ,{ctor: "_Tuple2"
                                                         ,_0: "height"
                                                         ,_1: $Util.px(diffButtonH)}]))]),
      _L.fromArray([$Html.text($Basics.toString(difficulty))]));
   };
   var inRowsOfSize = function (k) {
      return function ($) {
         return $List.map($Html.div(_L.fromArray([])))($Util.groupsOf(k)($));
      };
   };
   var difficultyButtonsDiv = $Html.div(_L.fromArray([]))(inRowsOfSize(2)(A2($List.map,
   difficultyButton,
   _L.fromArray([$GameTypes.S
                ,$GameTypes.M
                ,$GameTypes.L
                ,$GameTypes.XL]))));
   var withOpacity = F2(function (o,
   elt) {
      return A2($Html.toElement,
      $Graphics$Element.widthOf(elt),
      $Graphics$Element.heightOf(elt))(A2($Html.div,
      _L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                         ,_0: "opacity"
                                                         ,_1: $Basics.toString(o)}]))]),
      _L.fromArray([$Html.fromElement(elt)])));
   });
   var asTextWithStyle = F2(function (sty,
   x) {
      return $Graphics$Element.centered(A2($Text.style,
      sty,
      $Text.fromString($Basics.toString(x))));
   });
   var buttonArt = function () {
      var arc = F2(function (r,a) {
         return function () {
            var n = 50;
            var t = a / n;
            var f = function (i) {
               return {ctor: "_Tuple2"
                      ,_0: r * $Basics.cos(t * i)
                      ,_1: r * $Basics.sin(t * i)};
            };
            return A2($List.map,
            f,
            _L.range(0,n - 1));
         }();
      });
      var thickness = 5;
      var thick = function (c) {
         return function () {
            var sty = $Graphics$Collage.solid(c);
            return _U.replace([["width"
                               ,thickness]],
            sty);
         }();
      };
      var $ = {ctor: "_Tuple2"
              ,_0: 80
              ,_1: 80},
      w = $._0,
      h = $._1;
      var r = 0.9 * A2($Basics.min,
      w,
      h) / 2;
      var arrow = F2(function (a,l) {
         return function () {
            var r$ = (r - 3) * A2($Basics.min,
            1,
            (20 + l) / $Config.maxTransLen);
            return A2($Graphics$Collage.groupTransform,
            $Transform2D.rotation(a),
            _L.fromArray([A2($Graphics$Collage.traced,
                         thick($Color.black),
                         A2($Graphics$Collage.segment,
                         {ctor: "_Tuple2"
                         ,_0: 0 - r$
                         ,_1: 0},
                         {ctor: "_Tuple2",_0: r$,_1: 0}))
                         ,$Graphics$Collage.moveX(r$ - 7)(A2($Graphics$Collage.filled,
                         $Color.black,
                         A2($Graphics$Collage.ngon,
                         3,
                         10)))]));
         }();
      });
      var rotArc = function (a) {
         return function () {
            var r$ = r - 3;
            var arr = $Graphics$Collage.group(_L.fromArray([A2($Graphics$Collage.traced,
                                                           thick($Style.rotateArcColor),
                                                           A2(arc,r$,a))
                                                           ,$Graphics$Collage.groupTransform($Transform2D.rotation(a))($Util.sing($Graphics$Collage.moveX(r$)($Graphics$Collage.rotate((0 - $Basics.pi) / 6 + (_U.cmp(a,
                                                           0) < 0 ? $Basics.pi : 0))($Graphics$Collage.filled($Style.rotateArcColor)(A2($Graphics$Collage.ngon,
                                                           3,
                                                           10))))))]));
            return _U.cmp($Basics.abs(a),
            $Basics.pi) < 0 ? $Graphics$Collage.group(_L.fromArray([arr
                                                                   ,A2($Graphics$Collage.groupTransform,
                                                                   $Transform2D.rotation($Basics.pi),
                                                                   _L.fromArray([arr]))])) : arr;
         }();
      };
      var refLine = function (a) {
         return $Graphics$Collage.rotate(a)($Graphics$Collage.group(_L.fromArray([A2($Graphics$Collage.traced,
         thick($Color.green),
         A2($Graphics$Collage.segment,
         {ctor: "_Tuple2"
         ,_0: 0 - r
         ,_1: 0},
         {ctor: "_Tuple2"
         ,_0: r
         ,_1: 0}))])));
      };
      var fractionArt = function (x) {
         return function () {
            var sty = $Style.defTextStyle(22);
            var sty$ = _U.replace([["color"
                                   ,$Color.black]
                                  ,["bold",true]],
            sty);
            var $ = $Ratio.split(x),
            p = $._0,
            q = $._1;
            return _U.eq(q,
            1) ? $Graphics$Collage.toForm(A2(asTextWithStyle,
            _U.replace([["height"
                        ,$Maybe.Just(30)]],
            sty$),
            p)) : function () {
               var sty = function () {
                  var s = $Graphics$Collage.solid($Color.black);
                  return _U.replace([["width",2]],
                  s);
               }();
               var d = $Basics.sqrt(Math.pow(w,
               2) + Math.pow(h,2)) / 10;
               var d$ = 0.9 * d;
               var slash = A2($Graphics$Collage.traced,
               sty,
               A2($Graphics$Collage.segment,
               {ctor: "_Tuple2"
               ,_0: 0 - d
               ,_1: 0 - d},
               {ctor: "_Tuple2",_0: d,_1: d}));
               return $Graphics$Collage.group(_L.fromArray([$Graphics$Collage.move({ctor: "_Tuple2"
                                                                                   ,_0: 0 - d$
                                                                                   ,_1: d$})($Graphics$Collage.toForm(A2(asTextWithStyle,
                                                           sty$,
                                                           $Basics.abs(p))))
                                                           ,slash
                                                           ,$Graphics$Collage.move({ctor: "_Tuple2"
                                                                                   ,_0: d$
                                                                                   ,_1: 0 - d$})($Graphics$Collage.toForm(A2(asTextWithStyle,
                                                           sty$,
                                                           q)))]));
            }();
         }();
      };
      return function (t) {
         return function () {
            switch (t.ctor)
            {case "Identity":
               return $Graphics$Collage.group(_L.fromArray([]));
               case "Reflection":
               return refLine($Util.normalizeAngle(t._0));
               case "Rotation":
               return $Graphics$Collage.group(_L.fromArray([rotArc($Util.normalizeAngle(2 * $Basics.pi * $Ratio.toFloat(t._0)))
                                                           ,fractionArt(t._0)]));
               case "Translation":
               switch (t._0.ctor)
                 {case "_Tuple2":
                    return A2(arrow,
                      A2($Basics.atan2,
                      t._0._1,
                      t._0._0),
                      $Basics.sqrt(Math.pow(t._0._0,
                      2) + Math.pow(t._0._1,2)));}
                 break;}
            _U.badCase($moduleName,
            "between lines 216 and 220");
         }();
      };
   }();
   var transButtons = function (lev) {
      return function () {
         var n = function () {
            var _v8 = lev.availableMoves;
            switch (_v8.ctor)
            {case "::":
               return $List.length(_v8._0);
               case "[]": return 0;}
            _U.badCase($moduleName,
            "on line 225, column 11 to 70");
         }();
         var $ = {ctor: "_Tuple2"
                 ,_0: 80
                 ,_1: 80},
         buttonW = $._0,
         buttonH = $._1;
         var moveButton = function (m) {
            return $Html.div(_L.fromArray([A2($Html$Events.onClick,
                                          $Inputs.clickMoveChan.address,
                                          $Maybe.Just(m))
                                          ,A2($Html$Events.onMouseEnter,
                                          $Inputs.hoverMoveChan.address,
                                          $Maybe.Just(m))
                                          ,A2($Html$Events.onMouseLeave,
                                          $Inputs.hoverMoveChan.address,
                                          $Maybe.Nothing)
                                          ,$Html$Attributes.$class("movebutton")]))(A4($List.map3,
            F3(function (i,c,t) {
               return A2($Html.div,
               _L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                                  ,_0: "height"
                                                                  ,_1: $Util.px(buttonH)}
                                                                 ,{ctor: "_Tuple2"
                                                                  ,_0: "width"
                                                                  ,_1: $Util.px(buttonW)}
                                                                 ,{ctor: "_Tuple2"
                                                                  ,_0: "backgroundColor"
                                                                  ,_1: $Util.colorStr(c)}]))
                            ,$Html$Attributes.$class(_U.eq(i,
                            0) ? "top-isom isom" : _U.eq(i,
                            n - 1) ? "bottom-isom isom" : "isom")]),
               _L.fromArray([$Html.fromElement(A3($Graphics$Collage.collage,
               buttonW,
               buttonH,
               _L.fromArray([buttonArt(t)])))]));
            }),
            _L.range(0,n - 1),
            $Config.colors,
            m));
         };
         return $Html.div(_L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                                             ,_0: "textAlign"
                                                                             ,_1: "center"}]))]))($List.map(moveButton)(lev.availableMoves));
      }();
   };
   var withButtons = F2(function (mainScreen,
   s) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([mainScreen
                   ,A3($Html.toElement,
                   $Config.w,
                   300,
                   transButtons(s.currLevel))]));
   });
   var asText$ = F2(function (n,
   x) {
      return $Graphics$Element.centered(A2($Text.style,
      $Style.defTextStyle(n),
      $Text.fromString($Basics.toString(x))));
   });
   var axes = function () {
      var mk = function (_v11) {
         return function () {
            switch (_v11.ctor)
            {case "_Tuple2":
               return A2($Graphics$Collage.traced,
                 $Graphics$Collage.dotted($Color.black),
                 A2($Graphics$Collage.segment,
                 {ctor: "_Tuple2"
                 ,_0: 0 - _v11._0
                 ,_1: 0 - _v11._1},
                 {ctor: "_Tuple2"
                 ,_0: _v11._0
                 ,_1: _v11._1}));}
            _U.badCase($moduleName,
            "on line 133, column 19 to 70");
         }();
      };
      return $Graphics$Collage.group(_L.fromArray([mk({ctor: "_Tuple2"
                                                      ,_0: $Config.w / 2
                                                      ,_1: 0})
                                                  ,mk({ctor: "_Tuple2"
                                                      ,_0: 0
                                                      ,_1: $Config.h / 2})]));
   }();
   var titleScreen = $Graphics$Element.color($Style.fadeColor)(A3($Graphics$Element.container,
   $Config.w,
   $Config.totalHeight,
   $Graphics$Element.middle)(A2($Html.toElement,
   $Config.w,
   200)(A2($Html.div,
   _L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                      ,_0: "marginTop"
                                                      ,_1: "-40px"}]))]),
   _L.fromArray([A2($Html.div,
                _L.fromArray([$Html$Attributes.id("titletext")]),
                _L.fromArray([$Html.text("Short Words")]))
                ,A2($Html.p,
                _L.fromArray([$Html$Attributes.id("explanationtext")]),
                _L.fromArray([$Html.text("The goal of the game is to get all R\'s in the same position using the moves available. The icons on the buttons indicate what effect they have.")]))
                ,A2($Html.div,
                _L.fromArray([A2($Html$Events.onClick,
                             $Inputs.startGameChan.address,
                             {ctor: "_Tuple0"})
                             ,$Html$Attributes.$class("swbutton")]),
                _L.fromArray([$Html.text("Play")]))])))));
   var centeredWithWidth = F2(function (w,
   e) {
      return A4($Graphics$Element.container,
      w,
      $Graphics$Element.heightOf(e),
      $Graphics$Element.middle,
      e);
   });
   var chooseDifficultyScreen = function (totalScore) {
      return function () {
         var arrow = centeredWithWidth($Config.w)(A2($Graphics$Collage.collage,
         200,
         40)($Util.sing($Graphics$Collage.group(_L.fromArray([$Graphics$Collage.filled($Color.black)(A2($Graphics$Collage.rect,
                                                             90,
                                                             4))
                                                             ,$Graphics$Collage.moveX(40)($Graphics$Collage.filled($Color.black)(A2($Graphics$Collage.ngon,
                                                             3,
                                                             10)))])))));
         var sty = $Style.defTextStyle(30);
         var sel = centeredWithWidth($Config.w)($Graphics$Element.centered($Text.style(sty)($Text.fromString("Select a difficulty"))));
         return A2($Graphics$Element.flow,
         $Graphics$Element.down,
         _L.fromArray([A2($Graphics$Element.spacer,
                      $Config.w,
                      50)
                      ,sel
                      ,arrow]));
      }();
   };
   var loseAnimEnds = function (state) {
      return $Time.delay(loseAnimDuration)(A3($Util.filterMap,
      function (s) {
         return function () {
            var _v15 = s.levelState.endState;
            switch (_v15.ctor)
            {case "End":
               switch (_v15._0.ctor)
                 {case "Lose":
                    switch (_v15._1.ctor)
                      {case "Havent":
                         return $Maybe.Just(A2($Signal.send,
                           $Inputs.setEndStateChan.address,
                           $GameTypes.Normal));}
                      break;}
                 break;}
            return $Maybe.Nothing;
         }();
      },
      A2($Signal.send,
      $Inputs.setEndStateChan.address,
      $GameTypes.Normal),
      state));
   };
   var FreshLevelE = function (a) {
      return {ctor: "FreshLevelE"
             ,_0: a};
   };
   var LoseE = function (a) {
      return {ctor: "LoseE",_0: a};
   };
   var WinE = function (a) {
      return {ctor: "WinE",_0: a};
   };
   var SimpleMoveE = function (a) {
      return {ctor: "SimpleMoveE"
             ,_0: a};
   };
   var animEvents = F2(function (updates,
   state) {
      return A3($Signal.map2,
      F2(function (u,s) {
         return function () {
            var ls = s.levelState;
            return function () {
               switch (u.ctor)
               {case "Clicked":
                  return function () {
                       var _v22 = ls.endState;
                       switch (_v22.ctor)
                       {case "End":
                          switch (_v22._1.ctor)
                            {case "Have":
                               return $Maybe.Nothing;}
                            switch (_v22._0.ctor)
                            {case "Lose":
                               switch (_v22._1.ctor)
                                 {case "Havent":
                                    return $Maybe.Just(LoseE(_v22._0._0));}
                                 break;
                               case "Win":
                               switch (_v22._1.ctor)
                                 {case "Havent":
                                    return $Maybe.Just(WinE(_v22._0._0));}
                                 break;}
                            break;
                          case "Normal":
                          return $Maybe.Just(SimpleMoveE({_: {}
                                                         ,move: u._0
                                                         ,movesLeft: ls.movesLeft
                                                         ,pre: ls.preMove}));}
                       _U.badCase($moduleName,
                       "between lines 67 and 74");
                    }();
                  case "PlayLevelOfDifficulty":
                  return $Maybe.Just(FreshLevelE({_: {}
                                                 ,init: s.currLevel.initial
                                                 ,maxMoves: s.currLevel.maxMoves}));
                  case "ResetLevel":
                  return $Maybe.Just(FreshLevelE({_: {}
                                                 ,init: s.currLevel.initial
                                                 ,maxMoves: s.currLevel.maxMoves}));}
               return $Maybe.Nothing;
            }();
         }();
      }),
      updates,
      state);
   });
   var anR = function (color) {
      return function () {
         var sty = $Style.defTextStyle(55);
         return $Graphics$Collage.toForm($Graphics$Element.centered(A2($Text.style,
         _U.replace([["bold",true]
                    ,["color",color]],
         sty),
         $Text.fromString("R"))));
      }();
   };
   var formText = function (sty) {
      return function ($) {
         return $Graphics$Collage.toForm($Graphics$Element.centered($Text.style(sty)($Text.fromString($))));
      };
   };
   var movesLeftCircle = function (n) {
      return function () {
         var sty = $Style.defTextStyle(40);
         var r = 30;
         return $Graphics$Collage.move({ctor: "_Tuple2"
                                       ,_0: -200
                                       ,_1: 200})($Graphics$Collage.group(_L.fromArray([A2($Graphics$Collage.filled,
                                                                                       $Style.movesLeftCircleColor,
                                                                                       $Graphics$Collage.circle(r))
                                                                                       ,A2(formText,
                                                                                       _U.replace([["bold"
                                                                                                   ,true]
                                                                                                  ,["color"
                                                                                                   ,$Color.black]],
                                                                                       sty),
                                                                                       $Basics.toString(n))])));
      }();
   };
   var plane = function (s) {
      return A3($Graphics$Collage.collage,
      $Config.w,
      $Config.h,
      _L.fromArray([axes
                   ,$Graphics$Collage.group(A3($List.map2,
                   F2(function (c,t) {
                      return A2($Graphics$Collage.groupTransform,
                      t,
                      _L.fromArray([anR(c)]));
                   }),
                   $Config.colors,
                   s.currTranses))
                   ,movesLeftCircle(s.movesLeft)
                   ,resetButtonForm]));
   };
   var planePiece = function (d) {
      return A2($Piece.map,
      function (t) {
         return plane({_: {}
                      ,currTranses: t
                      ,movesLeft: d.movesLeft});
      },
      A2($Move.interpret,
      d.move,
      d.pre));
   };
   var winAnim = function (d) {
      return function () {
         var fadeTime = 1 * $Time.second;
         var winScreen = function (t) {
            return A2($Html.toElement,
            $Config.w,
            $Config.h)(A2($Html.div,
            _L.fromArray([$Html$Attributes.id("win-screen")
                         ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                               ,_0: "opacity"
                                                               ,_1: $Basics.toString(t / fadeTime)}]))]),
            _L.fromArray([A2($Html.div,
            _L.fromArray([$Html$Attributes.id("win-screen-inner")]),
            _L.fromArray([A2($Html.div,
                         _L.fromArray([]),
                         _L.fromArray([A2($Html.h1,
                                      _L.fromArray([]),
                                      _L.fromArray([$Html.text("You win!")]))
                                      ,A2($Html.span,
                                      _L.fromArray([$Html$Attributes.$class("score")]),
                                      _L.fromArray([$Html.text(A2($Basics._op["++"],
                                      "Score: ",
                                      $Basics.toString(d.totalScore)))]))]))
                         ,A2($Html.div,
                         _L.fromArray([]),
                         _L.fromArray([facebookButton
                                      ,A2(tweetButton,
                                      d.difficulty,
                                      d.totalScore)]))]))])));
         };
         return $Piece.sustain(A2($Piece$Infix._op["+>"],
         planePiece(d),
         function (p) {
            return A2($Piece$Infix._op["<>"],
            A2($Piece.stayFor,
            1 / 2 * $Time.second,
            p),
            A2($Piece.$for,
            fadeTime,
            function (t) {
               return A2($Graphics$Element.flow,
               $Graphics$Element.inward,
               _L.fromArray([winScreen(t),p]));
            }));
         }));
      }();
   };
   var animate = function (e) {
      return function () {
         switch (e.ctor)
         {case "FreshLevelE":
            return $Piece.stayForever(plane({_: {}
                                            ,currTranses: e._0.init
                                            ,movesLeft: e._0.maxMoves}));
            case "LoseE":
            return function () {
                 var flashRed = A2($Piece.stayFor,
                 1 / 4 * $Time.second,
                 A3($Graphics$Collage.collage,
                 $Config.w,
                 $Config.h,
                 _L.fromArray([A2($Graphics$Collage.filled,
                 $Color.red,
                 A2($Graphics$Collage.rect,
                 $Config.w,
                 $Config.h))])));
                 return A2($Piece$Infix._op["+>"],
                 planePiece(_U.insert("movesLeft",
                 0,
                 e._0)),
                 function (p) {
                    return A2($Piece$Infix._op["<>"],
                    A2($Piece$Infix._op["<>"],
                    A2($Piece$Infix._op["<>"],
                    A2($Piece$Infix._op["<>"],
                    A2($Piece.stayFor,
                    1 / 5 * $Time.second,
                    p),
                    flashRed),
                    A2($Piece.stayFor,
                    1 / 5 * $Time.second,
                    p)),
                    flashRed),
                    $Piece.stayForever(plane({_: {}
                                             ,currTranses: e._0.init
                                             ,movesLeft: e._0.maxMoves})));
                 });
              }();
            case "SimpleMoveE":
            return $Piece.sustain(planePiece(e._0));
            case "WinE":
            return winAnim(e._0);}
         _U.badCase($moduleName,
         "between lines 80 and 95");
      }();
   };
   var animations = F3(function (openingScreen,
   updates,
   state) {
      return A3($Util.filterMap,
      $Maybe.map(animate),
      $Piece.stayForever(openingScreen),
      A2(animEvents,updates,state));
   });
   var withBorder = F3(function (b,
   c,
   e) {
      return A2($Graphics$Element.color,
      c,
      A4($Graphics$Element.container,
      $Graphics$Element.widthOf(e) + 2 * b,
      $Graphics$Element.heightOf(e) + 2 * b,
      $Graphics$Element.middle,
      e));
   });
   var withAlpha = F2(function (a,
   c) {
      return function () {
         var $ = $Color.toRgb(c),
         red = $.red,
         green = $.green,
         blue = $.blue;
         return A4($Color.rgba,
         red,
         green,
         blue,
         a);
      }();
   });
   var hoverArt = F2(function (ls,
   m) {
      return function () {
         var rs = $Graphics$Collage.group(A2($List.map2,
         F2(function (c,t) {
            return A2($Graphics$Collage.groupTransform,
            t,
            _L.fromArray([anR(withAlpha(0.5)($Style.lighten(c)))]));
         }),
         $Config.colors)($Piece.finalValue(A2($Move.interpret,
         m,
         ls.postMove))));
         return $Graphics$Element.color($Color.white)(A3($Graphics$Collage.collage,
         $Config.w,
         $Config.h,
         _L.fromArray([axes
                      ,movesLeftCircle(ls.movesLeft)
                      ,rs
                      ,resetButtonForm])));
      }();
   });
   _elm.Draw.values = {_op: _op
                      ,withAlpha: withAlpha
                      ,withBorder: withBorder
                      ,formText: formText
                      ,anR: anR
                      ,SimpleMoveE: SimpleMoveE
                      ,WinE: WinE
                      ,LoseE: LoseE
                      ,FreshLevelE: FreshLevelE
                      ,loseAnimEnds: loseAnimEnds
                      ,animEvents: animEvents
                      ,animate: animate
                      ,animations: animations
                      ,hoverArt: hoverArt
                      ,centeredWithWidth: centeredWithWidth
                      ,titleScreen: titleScreen
                      ,axes: axes
                      ,movesLeftCircle: movesLeftCircle
                      ,plane: plane
                      ,asText$: asText$
                      ,asTextWithStyle: asTextWithStyle
                      ,buttonArt: buttonArt
                      ,transButtons: transButtons
                      ,planePiece: planePiece
                      ,withOpacity: withOpacity
                      ,inRowsOfSize: inRowsOfSize
                      ,diffButtonW: diffButtonW
                      ,diffButtonH: diffButtonH
                      ,difficultyButton: difficultyButton
                      ,difficultyButtonsDiv: difficultyButtonsDiv
                      ,chooseDifficultyScreen: chooseDifficultyScreen
                      ,socialButtonSize: socialButtonSize
                      ,facebookButton: facebookButton
                      ,tweetButton: tweetButton
                      ,winAnim: winAnim
                      ,withButtons: withButtons
                      ,loseAnimDuration: loseAnimDuration
                      ,resetButton: resetButton
                      ,difficultyButtonsOverlay: difficultyButtonsOverlay
                      ,overlayDifficultyButton: overlayDifficultyButton
                      ,resetButtonForm: resetButtonForm};
   return _elm.Draw.values;
};
Elm.Easing = Elm.Easing || {};
Elm.Easing.make = function (_elm) {
   "use strict";
   _elm.Easing = _elm.Easing || {};
   if (_elm.Easing.values)
   return _elm.Easing.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Easing",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Time = Elm.Time.make(_elm);
   var cycle = F3(function (animation,
   d,
   t) {
      return A2(animation,
      1,
      t / d - $Basics.toFloat($Basics.floor(t / d)));
   });
   var flip = F2(function (easing,
   time) {
      return easing(1 - time);
   });
   var retour = F2(function (easing,
   time) {
      return _U.cmp(time,
      0.5) < 0 ? easing(time * 2) : A2(flip,
      easing,
      (time - 0.5) * 2);
   });
   var invert = F2(function (easing,
   time) {
      return 1 - easing(1 - time);
   });
   var inOut = F3(function (e1,
   e2,
   time) {
      return _U.cmp(time,
      0.5) < 0 ? e1(time * 2) / 2 : 0.5 + e2((time - 0.5) * 2) / 2;
   });
   var easeInElastic = function (time) {
      return function () {
         var t$ = time - 1;
         var p = 0.3;
         var s = 7.5e-2;
         return 0 - Math.pow(2,
         10 * t$) * $Basics.sin((t$ - s) * (2 * $Basics.pi) / p);
      }();
   };
   var easeOutElastic = invert(easeInElastic);
   var easeInOutElastic = A2(inOut,
   easeInElastic,
   easeOutElastic);
   var easeOutBounce = function (time) {
      return function () {
         var t4 = time - 2.65 / 2.75;
         var t3 = time - 2.25 / 2.75;
         var t2 = time - 1.5 / 2.75;
         var a = 7.5625;
         return _U.cmp(time,
         1 / 2.75) < 0 ? a * time * time : _U.cmp(time,
         2 / 2.75) < 0 ? a * t2 * t2 + 0.75 : _U.cmp(time,
         2.5 / 2.75) < 0 ? a * t3 * t3 + 0.9375 : a * t4 * t4 + 0.984375;
      }();
   };
   var easeInBounce = invert(easeOutBounce);
   var easeInOutBounce = A2(inOut,
   easeInBounce,
   easeOutBounce);
   var easeInBack = function (time) {
      return time * time * (2.70158 * time - 1.70158);
   };
   var easeOutBack = invert(easeInBack);
   var easeInOutBack = A2(inOut,
   easeInBack,
   easeOutBack);
   var easeOutCirc = function (time) {
      return $Basics.sqrt(1 - Math.pow(time - 1,
      2));
   };
   var easeInCirc = invert(easeOutCirc);
   var easeInOutCirc = A2(inOut,
   easeInCirc,
   easeOutCirc);
   var easeInExpo = function (time) {
      return Math.pow(2,
      10 * (time - 1));
   };
   var easeOutExpo = invert(easeInExpo);
   var easeInOutExpo = A2(inOut,
   easeInExpo,
   easeOutExpo);
   var easeOutSine = function (time) {
      return $Basics.sin(time * ($Basics.pi / 2));
   };
   var easeInSine = invert(easeOutSine);
   var easeInOutSine = A2(inOut,
   easeInSine,
   easeOutSine);
   var easeInQuint = function (time) {
      return Math.pow(time,5);
   };
   var easeOutQuint = invert(easeInQuint);
   var easeInOutQuint = A2(inOut,
   easeInQuint,
   easeOutQuint);
   var easeInQuart = function (time) {
      return Math.pow(time,4);
   };
   var easeOutQuart = invert(easeInQuart);
   var easeInOutQuart = A2(inOut,
   easeInQuart,
   easeOutQuart);
   var easeInCubic = function (time) {
      return Math.pow(time,3);
   };
   var easeOutCubic = invert(easeInCubic);
   var easeInOutCubic = A2(inOut,
   easeInCubic,
   easeOutCubic);
   var easeInQuad = function (time) {
      return Math.pow(time,2);
   };
   var easeOutQuad = invert(easeInQuad);
   var easeInOutQuad = A2(inOut,
   easeInQuad,
   easeOutQuad);
   var linear = $Basics.identity;
   var pair = F4(function (interpolate,
   _v0,
   _v1,
   v) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return {ctor: "_Tuple2"
                           ,_0: A3(interpolate,
                           _v0._0,
                           _v1._0,
                           v)
                           ,_1: A3(interpolate,
                           _v0._1,
                           _v1._1,
                           v)};}
                 _U.badCase($moduleName,
                 "on line 145, column 6 to 46");
              }();}
         _U.badCase($moduleName,
         "on line 145, column 6 to 46");
      }();
   });
   var $float = F3(function (from,
   to,
   v) {
      return from + (to - from) * v;
   });
   var point2d = F3(function (from,
   to,
   v) {
      return {_: {}
             ,x: A3($float,from.x,to.x,v)
             ,y: A3($float,from.y,to.y,v)};
   });
   var point3d = F3(function (from,
   to,
   v) {
      return {_: {}
             ,x: A3($float,from.x,to.x,v)
             ,y: A3($float,from.y,to.y,v)
             ,z: A3($float,from.z,to.z,v)};
   });
   var color = F3(function (from,
   to,
   v) {
      return function () {
         var float$ = F3(function (from,
         to,
         v) {
            return $Basics.round(A3($float,
            $Basics.toFloat(from),
            $Basics.toFloat(to),
            v));
         });
         var $ = {ctor: "_Tuple2"
                 ,_0: $Color.toRgb(from)
                 ,_1: $Color.toRgb(to)},
         rgb1 = $._0,
         rgb2 = $._1;
         var $ = {ctor: "_Tuple4"
                 ,_0: rgb1.red
                 ,_1: rgb1.green
                 ,_2: rgb1.blue
                 ,_3: rgb1.alpha},
         r1 = $._0,
         g1 = $._1,
         b1 = $._2,
         a1 = $._3;
         var $ = {ctor: "_Tuple4"
                 ,_0: rgb2.red
                 ,_1: rgb2.green
                 ,_2: rgb2.blue
                 ,_3: rgb2.alpha},
         r2 = $._0,
         g2 = $._1,
         b2 = $._2,
         a2 = $._3;
         return A4($Color.rgba,
         A3(float$,r1,r2,v),
         A3(float$,g1,g2,v),
         A3(float$,b1,b2,v),
         A3($float,a1,a2,v));
      }();
   });
   var bezier = F5(function (x1,
   y1,
   x2,
   y2,
   time) {
      return function () {
         var casteljau = function (ps) {
            return function () {
               switch (ps.ctor)
               {case "::": switch (ps._0.ctor)
                    {case "_Tuple2":
                       switch (ps._1.ctor)
                         {case "[]": return ps._0._1;}
                         break;}
                    break;}
               return casteljau(A3($List.map2,
               F2(function (x,y) {
                  return A4(pair,
                  $float,
                  x,
                  y,
                  time);
               }),
               ps,
               A2($Maybe.withDefault,
               _L.fromArray([]),
               $List.tail(ps))));
            }();
         };
         return casteljau(_L.fromArray([{ctor: "_Tuple2"
                                        ,_0: 0
                                        ,_1: 0}
                                       ,{ctor: "_Tuple2",_0: x1,_1: y1}
                                       ,{ctor: "_Tuple2",_0: x2,_1: y2}
                                       ,{ctor: "_Tuple2"
                                        ,_0: 1
                                        ,_1: 1}]));
      }();
   });
   var ease = F6(function (easing,
   interpolate,
   from,
   to,
   duration,
   time) {
      return A3(interpolate,
      from,
      to,
      easing(A2($Basics.min,
      time / duration,
      1)));
   });
   _elm.Easing.values = {_op: _op
                        ,ease: ease
                        ,$float: $float
                        ,point2d: point2d
                        ,point3d: point3d
                        ,color: color
                        ,pair: pair
                        ,cycle: cycle
                        ,invert: invert
                        ,retour: retour
                        ,inOut: inOut
                        ,flip: flip
                        ,bezier: bezier
                        ,linear: linear
                        ,easeInQuad: easeInQuad
                        ,easeOutQuad: easeOutQuad
                        ,easeInOutQuad: easeInOutQuad
                        ,easeInCubic: easeInCubic
                        ,easeOutCubic: easeOutCubic
                        ,easeInOutCubic: easeInOutCubic
                        ,easeInQuart: easeInQuart
                        ,easeOutQuart: easeOutQuart
                        ,easeInOutQuart: easeInOutQuart
                        ,easeInQuint: easeInQuint
                        ,easeOutQuint: easeOutQuint
                        ,easeInOutQuint: easeInOutQuint
                        ,easeInSine: easeInSine
                        ,easeOutSine: easeOutSine
                        ,easeInOutSine: easeInOutSine
                        ,easeInExpo: easeInExpo
                        ,easeOutExpo: easeOutExpo
                        ,easeInOutExpo: easeInOutExpo
                        ,easeInCirc: easeInCirc
                        ,easeOutCirc: easeOutCirc
                        ,easeInOutCirc: easeInOutCirc
                        ,easeInBack: easeInBack
                        ,easeOutBack: easeOutBack
                        ,easeInOutBack: easeInOutBack
                        ,easeInBounce: easeInBounce
                        ,easeOutBounce: easeOutBounce
                        ,easeInOutBounce: easeInOutBounce
                        ,easeInElastic: easeInElastic
                        ,easeOutElastic: easeOutElastic
                        ,easeInOutElastic: easeInOutElastic};
   return _elm.Easing.values;
};
Elm.GameTypes = Elm.GameTypes || {};
Elm.GameTypes.make = function (_elm) {
   "use strict";
   _elm.GameTypes = _elm.GameTypes || {};
   if (_elm.GameTypes.values)
   return _elm.GameTypes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "GameTypes",
   $Array = Elm.Array.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Random = Elm.Random.make(_elm);
   var Finished = {ctor: "Finished"};
   var ChooseLevel = {ctor: "ChooseLevel"};
   var PlayLevel = {ctor: "PlayLevel"};
   var TitleScreen = {ctor: "TitleScreen"};
   var NoOp = {ctor: "NoOp"};
   var SetEndState = function (a) {
      return {ctor: "SetEndState"
             ,_0: a};
   };
   var ResetLevel = {ctor: "ResetLevel"};
   var Unhovered = {ctor: "Unhovered"};
   var Hovered = function (a) {
      return {ctor: "Hovered"
             ,_0: a};
   };
   var SetTotalScore = function (a) {
      return {ctor: "SetTotalScore"
             ,_0: a};
   };
   var PlayLevelOfDifficulty = function (a) {
      return {ctor: "PlayLevelOfDifficulty"
             ,_0: a};
   };
   var Clicked = function (a) {
      return {ctor: "Clicked"
             ,_0: a};
   };
   var difficultyScore = function (d) {
      return function () {
         switch (d.ctor)
         {case "L": return 300;
            case "M": return 200;
            case "S": return 100;
            case "XL": return 400;}
         _U.badCase($moduleName,
         "between lines 64 and 68");
      }();
   };
   var XL = {ctor: "XL"};
   var L = {ctor: "L"};
   var M = {ctor: "M"};
   var S = {ctor: "S"};
   var AnimState = F2(function (a,
   b) {
      return {_: {}
             ,currTranses: a
             ,movesLeft: b};
   });
   var LevelState = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,endState: d
             ,movesLeft: a
             ,postMove: b
             ,preMove: c};
   });
   var End = F2(function (a,b) {
      return {ctor: "End"
             ,_0: a
             ,_1: b};
   });
   var Normal = {ctor: "Normal"};
   var Lose = function (a) {
      return {ctor: "Lose",_0: a};
   };
   var Win = function (a) {
      return {ctor: "Win",_0: a};
   };
   var WinData = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,difficulty: e
             ,move: b
             ,movesLeft: c
             ,pre: a
             ,totalScore: d};
   });
   var Havent = {ctor: "Havent"};
   var Have = {ctor: "Have"};
   var GameState = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,currLevel: b
             ,lastMove: c
             ,levelState: a
             ,seed: e
             ,totalScore: d};
   });
   var Level = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,availableMoves: a
             ,difficulty: d
             ,initial: c
             ,maxMoves: b};
   });
   _elm.GameTypes.values = {_op: _op
                           ,Level: Level
                           ,GameState: GameState
                           ,Have: Have
                           ,Havent: Havent
                           ,WinData: WinData
                           ,Win: Win
                           ,Lose: Lose
                           ,Normal: Normal
                           ,End: End
                           ,LevelState: LevelState
                           ,AnimState: AnimState
                           ,S: S
                           ,M: M
                           ,L: L
                           ,XL: XL
                           ,difficultyScore: difficultyScore
                           ,Clicked: Clicked
                           ,PlayLevelOfDifficulty: PlayLevelOfDifficulty
                           ,SetTotalScore: SetTotalScore
                           ,Hovered: Hovered
                           ,Unhovered: Unhovered
                           ,ResetLevel: ResetLevel
                           ,SetEndState: SetEndState
                           ,NoOp: NoOp
                           ,TitleScreen: TitleScreen
                           ,PlayLevel: PlayLevel
                           ,ChooseLevel: ChooseLevel
                           ,Finished: Finished};
   return _elm.GameTypes.values;
};
Elm.Generate = Elm.Generate || {};
Elm.Generate.make = function (_elm) {
   "use strict";
   _elm.Generate = _elm.Generate || {};
   if (_elm.Generate.values)
   return _elm.Generate.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Generate",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Config = Elm.Config.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Isom = Elm.Isom.make(_elm),
   $Iterator = Elm.Iterator.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm),
   $Util = Elm.Util.make(_elm);
   var rdup = function (g) {
      return A2($Random.pair,g,g);
   };
   var trans2Dclose = F2(function (a,
   b) {
      return _U.cmp(A2($Util.distTransform2D,
      a,
      b),
      1.0e-2) < 0;
   });
   var close = F2(function (xs,
   ys) {
      return $Util.and(A3($List.map2,
      trans2Dclose,
      xs,
      ys));
   });
   var timeout = 1000;
   var sInterpsUpTo = F2(function (n,
   gens) {
      return function () {
         var gens$ = $Iterator.fromList(gens);
         var go = function (k) {
            return _U.eq(k,
            0) ? A2($Iterator.upTil,
            1,
            function (_v0) {
               return function () {
                  return A2($List.repeat,
                  $Config.maxSheets,
                  $Transform2D.identity);
               }();
            }) : A2($Iterator.concatMap,
            function (m) {
               return A2($Iterator.map,
               $Move.sMultiply(m),
               gens$);
            },
            go(k - 1));
         };
         return go(n);
      }();
   });
   var isServiceableOfLength = function () {
      var hasLargeTranslationalPart = function (t) {
         return function () {
            var $ = $Util.translationalPart(t),
            x = $._0,
            y = $._1;
            return _U.cmp($Basics.abs(x),
            $Config.w / 2) > 0 || _U.cmp($Basics.abs(y),
            $Config.h / 2) > 0;
         }();
      };
      return F3(function (n,gens,m) {
         return A2($List.all,
         function ($) {
            return $Basics.not(hasLargeTranslationalPart($));
         },
         m) && A2($Iterator.all,
         function (i) {
            return A2($Iterator.all,
            function (m$) {
               return $Basics.not($Util.isDiagonal(A2($Move.sMultiply,
               m$,
               m)));
            },
            A2(sInterpsUpTo,i,gens));
         },
         A2($Iterator.range,0,n - 1));
      });
   }();
   var rreturn = function (x) {
      return $Random.customGenerator(function (s) {
         return {ctor: "_Tuple2"
                ,_0: x
                ,_1: s};
      });
   };
   var randThen = F2(function (g,
   f) {
      return $Random.customGenerator(function (s) {
         return function () {
            var $ = A2($Random.generate,
            g,
            s),
            x = $._0,
            s$ = $._1;
            return A2($Random.generate,
            f(x),
            s$);
         }();
      });
   });
   var rmap = F2(function (f,g) {
      return $Random.customGenerator(function (s) {
         return function () {
            var $ = A2($Random.generate,
            g,
            s),
            x = $._0,
            s$ = $._1;
            return {ctor: "_Tuple2"
                   ,_0: f(x)
                   ,_1: s$};
         }();
      });
   });
   var randomSInterpOfLength = F2(function (n,
   gens) {
      return function () {
         var gensArr = $Array.fromList(gens);
         return rmap($Util.foldl1($Move.sMultiply))(A2($Random.list,
         n,
         A2(rmap,
         function (i) {
            return function () {
               var _v2 = A2($Array.get,
               i,
               gensArr);
               switch (_v2.ctor)
               {case "Just": return _v2._0;}
               _U.badCase($moduleName,
               "on line 30, column 32 to 71");
            }();
         },
         A2($Random.$int,
         0,
         $Array.length(gensArr) - 1))));
      }();
   });
   var serviceableInitialState = F2(function (n,
   gens) {
      return function () {
         var invGens = A2($List.map,
         $List.map($Util.invert),
         gens);
         var go = function (k) {
            return _U.eq(k,
            0) ? rreturn($Maybe.Nothing) : A2(randThen,
            A2(randomSInterpOfLength,
            n,
            invGens),
            function (mInv) {
               return A3(isServiceableOfLength,
               n,
               gens,
               mInv) ? rreturn($Maybe.Just(mInv)) : go(k - 1);
            });
         };
         var salted = F2(function (k,m) {
            return rreturn($Maybe.Just(m));
         });
         return go(timeout);
      }();
   });
   var randomSign = function () {
      var interp = function (x) {
         return _U.cmp(x,
         0) > 0 ? $Basics.identity : $Basics.negate;
      };
      return A2(rmap,
      function ($) {
         return interp(function (x) {
            return 2 * x - 1;
         }($));
      },
      A2($Random.$int,0,1));
   }();
   var randomElem = function () {
      var getNth = F2(function (xs,
      n) {
         return function () {
            switch (xs.ctor)
            {case "::": return _U.eq(n,
                 0) ? xs._0 : A2(getNth,
                 xs._1,
                 n - 1);}
            _U.badCase($moduleName,
            "on line 120, column 21 to 83");
         }();
      });
      return function (xs) {
         return function () {
            var n = $List.length(xs);
            return A2(rmap,
            getNth(xs),
            A2($Random.$int,0,n - 1));
         }();
      };
   }();
   var randomIsom = function () {
      var randomReflection = A2(randThen,
      randomElem(_L.fromArray([2
                              ,3
                              ,4
                              ,8])),
      function (denom) {
         return A2(randThen,
         A2($Random.$int,0,denom - 1),
         function (num) {
            return rreturn($Isom.reflection(2 * $Basics.pi * $Basics.toFloat(num) / $Basics.toFloat(denom)));
         });
      });
      var randomRotation = A2(randThen,
      randomElem(_L.fromArray([2
                              ,3
                              ,4
                              ,8])),
      function (denom) {
         return A2(randThen,
         A2($Random.$int,1,denom - 1),
         function (num) {
            return rreturn($Isom.rotation(A2($Ratio.over,
            num,
            denom)));
         });
      });
      var randomTranslation = A2(randThen,
      randomElem(_L.fromArray([2
                              ,3
                              ,4
                              ,8])),
      function (denom) {
         return A2(randThen,
         A2($Random.$int,0,denom - 1),
         function (num) {
            return A2(randThen,
            A2($Random.$int,1,3),
            function (scale) {
               return function () {
                  var r = $Basics.toFloat(25 * (1 + scale));
                  var a = 2 * $Basics.pi * ($Basics.toFloat(num) / $Basics.toFloat(denom));
                  return rreturn($Isom.Translation({ctor: "_Tuple2"
                                                   ,_0: r * $Basics.cos(a)
                                                   ,_1: r * $Basics.sin(a)}));
               }();
            });
         });
      });
      return A2(randThen,
      A2($Random.$int,0,3),
      function (i) {
         return function () {
            switch (i)
            {case 0:
               return randomTranslation;
               case 1: return randomRotation;
               case 2: return randomReflection;
               case 3:
               return rreturn($Isom.identity);}
            _U.badCase($moduleName,
            "between lines 143 and 147");
         }();
      });
   }();
   var randomTrans = A2(rmap,
   $Isom.sInterpret,
   randomIsom);
   var randomMove = function (p) {
      return A2($Random.list,
      p,
      randomIsom);
   };
   var randomLevel = function (difficulty) {
      return function () {
         var $ = function () {
            switch (difficulty.ctor)
            {case "L":
               return {ctor: "_Tuple6"
                      ,_0: 3
                      ,_1: 5
                      ,_2: 3
                      ,_3: 4
                      ,_4: 2
                      ,_5: 2};
               case "M":
               return {ctor: "_Tuple6"
                      ,_0: 3
                      ,_1: 3
                      ,_2: 2
                      ,_3: 3
                      ,_4: 2
                      ,_5: 2};
               case "S":
               return {ctor: "_Tuple6"
                      ,_0: 2
                      ,_1: 3
                      ,_2: 2
                      ,_3: 3
                      ,_4: 2
                      ,_5: 2};
               case "XL":
               return {ctor: "_Tuple6"
                      ,_0: 4
                      ,_1: 7
                      ,_2: 3
                      ,_3: 4
                      ,_4: 2
                      ,_5: 2};}
            _U.badCase($moduleName,
            "between lines 152 and 157");
         }(),
         minMoves = $._0,
         maxMoves = $._1,
         minGens = $._2,
         maxGens = $._3,
         minSheets = $._4,
         maxSheets = $._5;
         return A2(randThen,
         A2($Random.$int,
         minMoves,
         maxMoves),
         function (numMoves) {
            return A2(randThen,
            A2($Random.$int,
            minGens,
            maxGens),
            function (numGens) {
               return A2(randThen,
               A2($Random.$int,
               minSheets,
               maxSheets),
               function (numSheets) {
                  return A2(randThen,
                  A2($Random.list,
                  numGens,
                  randomMove(numSheets)),
                  function (gens) {
                     return function () {
                        var gens$ = A2($List.filter,
                        function ($) {
                           return $Basics.not($List.all(F2(function (x,
                           y) {
                              return _U.eq(x,y);
                           })($Isom.identity))($));
                        },
                        gens);
                        return A2(randThen,
                        A2(serviceableInitialState,
                        numMoves,
                        A2($List.map,
                        $Move.sInterpret,
                        gens$)),
                        function (mtrans) {
                           return function () {
                              switch (mtrans.ctor)
                              {case "Just":
                                 return rreturn({_: {}
                                                ,availableMoves: gens$
                                                ,difficulty: difficulty
                                                ,initial: mtrans._0
                                                ,maxMoves: numMoves});
                                 case "Nothing":
                                 return randomLevel(difficulty);}
                              _U.badCase($moduleName,
                              "between lines 164 and 171");
                           }();
                        });
                     }();
                  });
               });
            });
         });
      }();
   };
   _elm.Generate.values = {_op: _op
                          ,rmap: rmap
                          ,randThen: randThen
                          ,rreturn: rreturn
                          ,randomSInterpOfLength: randomSInterpOfLength
                          ,sInterpsUpTo: sInterpsUpTo
                          ,isServiceableOfLength: isServiceableOfLength
                          ,randomTrans: randomTrans
                          ,timeout: timeout
                          ,serviceableInitialState: serviceableInitialState
                          ,trans2Dclose: trans2Dclose
                          ,close: close
                          ,rdup: rdup
                          ,randomSign: randomSign
                          ,randomElem: randomElem
                          ,randomIsom: randomIsom
                          ,randomMove: randomMove
                          ,randomLevel: randomLevel};
   return _elm.Generate.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 226, column 7 to 35");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FText = function (a) {
      return {ctor: "FText",_0: a};
   };
   var text = function (t) {
      return form(FText(t));
   };
   var FOutlinedText = F2(function (a,
   b) {
      return {ctor: "FOutlinedText"
             ,_0: a
             ,_1: b};
   });
   var outlinedText = F2(function (ls,
   t) {
      return form(A2(FOutlinedText,
      ls,
      t));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,collage: collage
                                  ,toForm: toForm
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,text: text
                                  ,outlinedText: outlinedText
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,rect: rect
                                  ,oval: oval
                                  ,square: square
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,polygon: polygon
                                  ,segment: segment
                                  ,path: path
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,defaultLine: defaultLine
                                  ,Form: Form
                                  ,LineStyle: LineStyle
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         A2($Maybe.withDefault,
         0,
         $List.maximum(ws)),
         A2($Maybe.withDefault,
         0,
         $List.maximum(hs)),
         A2(Flow,DOut,es));
      }();
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var maxOrZero = function (list) {
            return A2($Maybe.withDefault,
            0,
            $List.maximum(list));
         };
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DOut": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DUp": return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 362 and 373");
         }();
      }();
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   _elm.Graphics.Element.values = {_op: _op
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,leftAligned: leftAligned
                                  ,rightAligned: rightAligned
                                  ,centered: centered
                                  ,justified: justified
                                  ,show: show
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,color: color
                                  ,opacity: opacity
                                  ,link: link
                                  ,tag: tag
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,flow: flow
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,layers: layers
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,empty: empty
                                  ,spacer: spacer
                                  ,container: container
                                  ,middle: middle
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middleAt: middleAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values)
   return _elm.Html.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   _elm.Html.values = {_op: _op
                      ,node: node
                      ,text: text
                      ,toElement: toElement
                      ,fromElement: fromElement
                      ,body: body
                      ,section: section
                      ,nav: nav
                      ,article: article
                      ,aside: aside
                      ,h1: h1
                      ,h2: h2
                      ,h3: h3
                      ,h4: h4
                      ,h5: h5
                      ,h6: h6
                      ,header: header
                      ,footer: footer
                      ,address: address
                      ,main$: main$
                      ,p: p
                      ,hr: hr
                      ,pre: pre
                      ,blockquote: blockquote
                      ,ol: ol
                      ,ul: ul
                      ,li: li
                      ,dl: dl
                      ,dt: dt
                      ,dd: dd
                      ,figure: figure
                      ,figcaption: figcaption
                      ,div: div
                      ,a: a
                      ,em: em
                      ,strong: strong
                      ,small: small
                      ,s: s
                      ,cite: cite
                      ,q: q
                      ,dfn: dfn
                      ,abbr: abbr
                      ,time: time
                      ,code: code
                      ,$var: $var
                      ,samp: samp
                      ,kbd: kbd
                      ,sub: sub
                      ,sup: sup
                      ,i: i
                      ,b: b
                      ,u: u
                      ,mark: mark
                      ,ruby: ruby
                      ,rt: rt
                      ,rp: rp
                      ,bdi: bdi
                      ,bdo: bdo
                      ,span: span
                      ,br: br
                      ,wbr: wbr
                      ,ins: ins
                      ,del: del
                      ,img: img
                      ,iframe: iframe
                      ,embed: embed
                      ,object: object
                      ,param: param
                      ,video: video
                      ,audio: audio
                      ,source: source
                      ,track: track
                      ,canvas: canvas
                      ,svg: svg
                      ,math: math
                      ,table: table
                      ,caption: caption
                      ,colgroup: colgroup
                      ,col: col
                      ,tbody: tbody
                      ,thead: thead
                      ,tfoot: tfoot
                      ,tr: tr
                      ,td: td
                      ,th: th
                      ,form: form
                      ,fieldset: fieldset
                      ,legend: legend
                      ,label: label
                      ,input: input
                      ,button: button
                      ,select: select
                      ,datalist: datalist
                      ,optgroup: optgroup
                      ,option: option
                      ,textarea: textarea
                      ,keygen: keygen
                      ,output: output
                      ,progress: progress
                      ,meter: meter
                      ,details: details
                      ,summary: summary
                      ,menuitem: menuitem
                      ,menu: menu};
   return _elm.Html.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Attributes",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var attribute = $VirtualDom.attribute;
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,
   string) {
      return A2(property,
      name,
      $Json$Encode.string(string));
   });
   var $class = function (name) {
      return A2(stringProperty,
      "className",
      name);
   };
   var id = function (name) {
      return A2(stringProperty,
      "id",
      name);
   };
   var title = function (name) {
      return A2(stringProperty,
      "title",
      name);
   };
   var accesskey = function ($char) {
      return A2(stringProperty,
      "accesskey",
      $String.fromList(_L.fromArray([$char])));
   };
   var contextmenu = function (value) {
      return A2(stringProperty,
      "contextmenu",
      value);
   };
   var dir = function (value) {
      return A2(stringProperty,
      "dir",
      value);
   };
   var draggable = function (value) {
      return A2(stringProperty,
      "draggable",
      value);
   };
   var dropzone = function (value) {
      return A2(stringProperty,
      "dropzone",
      value);
   };
   var itemprop = function (value) {
      return A2(stringProperty,
      "itemprop",
      value);
   };
   var lang = function (value) {
      return A2(stringProperty,
      "lang",
      value);
   };
   var tabindex = function (n) {
      return A2(stringProperty,
      "tabIndex",
      $Basics.toString(n));
   };
   var charset = function (value) {
      return A2(stringProperty,
      "charset",
      value);
   };
   var content = function (value) {
      return A2(stringProperty,
      "content",
      value);
   };
   var httpEquiv = function (value) {
      return A2(stringProperty,
      "httpEquiv",
      value);
   };
   var language = function (value) {
      return A2(stringProperty,
      "language",
      value);
   };
   var src = function (value) {
      return A2(stringProperty,
      "src",
      value);
   };
   var height = function (value) {
      return A2(stringProperty,
      "height",
      $Basics.toString(value));
   };
   var width = function (value) {
      return A2(stringProperty,
      "width",
      $Basics.toString(value));
   };
   var alt = function (value) {
      return A2(stringProperty,
      "alt",
      value);
   };
   var preload = function (value) {
      return A2(stringProperty,
      "preload",
      value);
   };
   var poster = function (value) {
      return A2(stringProperty,
      "poster",
      value);
   };
   var kind = function (value) {
      return A2(stringProperty,
      "kind",
      value);
   };
   var srclang = function (value) {
      return A2(stringProperty,
      "srclang",
      value);
   };
   var sandbox = function (value) {
      return A2(stringProperty,
      "sandbox",
      value);
   };
   var srcdoc = function (value) {
      return A2(stringProperty,
      "srcdoc",
      value);
   };
   var type$ = function (value) {
      return A2(stringProperty,
      "type",
      value);
   };
   var value = function (value) {
      return A2(stringProperty,
      "value",
      value);
   };
   var placeholder = function (value) {
      return A2(stringProperty,
      "placeholder",
      value);
   };
   var accept = function (value) {
      return A2(stringProperty,
      "accept",
      value);
   };
   var acceptCharset = function (value) {
      return A2(stringProperty,
      "acceptCharset",
      value);
   };
   var action = function (value) {
      return A2(stringProperty,
      "action",
      value);
   };
   var autocomplete = function (bool) {
      return A2(stringProperty,
      "autocomplete",
      bool ? "on" : "off");
   };
   var autosave = function (value) {
      return A2(stringProperty,
      "autosave",
      value);
   };
   var enctype = function (value) {
      return A2(stringProperty,
      "enctype",
      value);
   };
   var formaction = function (value) {
      return A2(stringProperty,
      "formaction",
      value);
   };
   var list = function (value) {
      return A2(stringProperty,
      "list",
      value);
   };
   var minlength = function (n) {
      return A2(stringProperty,
      "minLength",
      $Basics.toString(n));
   };
   var maxlength = function (n) {
      return A2(stringProperty,
      "maxLength",
      $Basics.toString(n));
   };
   var method = function (value) {
      return A2(stringProperty,
      "method",
      value);
   };
   var name = function (value) {
      return A2(stringProperty,
      "name",
      value);
   };
   var pattern = function (value) {
      return A2(stringProperty,
      "pattern",
      value);
   };
   var size = function (n) {
      return A2(stringProperty,
      "size",
      $Basics.toString(n));
   };
   var $for = function (value) {
      return A2(stringProperty,
      "htmlFor",
      value);
   };
   var form = function (value) {
      return A2(stringProperty,
      "form",
      value);
   };
   var max = function (value) {
      return A2(stringProperty,
      "max",
      value);
   };
   var min = function (value) {
      return A2(stringProperty,
      "min",
      value);
   };
   var step = function (n) {
      return A2(stringProperty,
      "step",
      n);
   };
   var cols = function (n) {
      return A2(stringProperty,
      "cols",
      $Basics.toString(n));
   };
   var rows = function (n) {
      return A2(stringProperty,
      "rows",
      $Basics.toString(n));
   };
   var wrap = function (value) {
      return A2(stringProperty,
      "wrap",
      value);
   };
   var usemap = function (value) {
      return A2(stringProperty,
      "useMap",
      value);
   };
   var shape = function (value) {
      return A2(stringProperty,
      "shape",
      value);
   };
   var coords = function (value) {
      return A2(stringProperty,
      "coords",
      value);
   };
   var challenge = function (value) {
      return A2(stringProperty,
      "challenge",
      value);
   };
   var keytype = function (value) {
      return A2(stringProperty,
      "keytype",
      value);
   };
   var align = function (value) {
      return A2(stringProperty,
      "align",
      value);
   };
   var cite = function (value) {
      return A2(stringProperty,
      "cite",
      value);
   };
   var href = function (value) {
      return A2(stringProperty,
      "href",
      value);
   };
   var target = function (value) {
      return A2(stringProperty,
      "target",
      value);
   };
   var downloadAs = function (value) {
      return A2(stringProperty,
      "download",
      value);
   };
   var hreflang = function (value) {
      return A2(stringProperty,
      "hreflang",
      value);
   };
   var media = function (value) {
      return A2(stringProperty,
      "media",
      value);
   };
   var ping = function (value) {
      return A2(stringProperty,
      "ping",
      value);
   };
   var rel = function (value) {
      return A2(stringProperty,
      "rel",
      value);
   };
   var datetime = function (value) {
      return A2(stringProperty,
      "datetime",
      value);
   };
   var pubdate = function (value) {
      return A2(stringProperty,
      "pubdate",
      value);
   };
   var start = function (n) {
      return A2(stringProperty,
      "start",
      $Basics.toString(n));
   };
   var colspan = function (n) {
      return A2(stringProperty,
      "colSpan",
      $Basics.toString(n));
   };
   var headers = function (value) {
      return A2(stringProperty,
      "headers",
      value);
   };
   var rowspan = function (n) {
      return A2(stringProperty,
      "rowSpan",
      $Basics.toString(n));
   };
   var scope = function (value) {
      return A2(stringProperty,
      "scope",
      value);
   };
   var manifest = function (value) {
      return A2(stringProperty,
      "manifest",
      value);
   };
   var boolProperty = F2(function (name,
   bool) {
      return A2(property,
      name,
      $Json$Encode.bool(bool));
   });
   var hidden = function (bool) {
      return A2(boolProperty,
      "hidden",
      bool);
   };
   var contenteditable = function (bool) {
      return A2(boolProperty,
      "contentEditable",
      bool);
   };
   var spellcheck = function (bool) {
      return A2(boolProperty,
      "spellcheck",
      bool);
   };
   var async = function (bool) {
      return A2(boolProperty,
      "async",
      bool);
   };
   var defer = function (bool) {
      return A2(boolProperty,
      "defer",
      bool);
   };
   var scoped = function (bool) {
      return A2(boolProperty,
      "scoped",
      bool);
   };
   var autoplay = function (bool) {
      return A2(boolProperty,
      "autoplay",
      bool);
   };
   var controls = function (bool) {
      return A2(boolProperty,
      "controls",
      bool);
   };
   var loop = function (bool) {
      return A2(boolProperty,
      "loop",
      bool);
   };
   var $default = function (bool) {
      return A2(boolProperty,
      "default",
      bool);
   };
   var seamless = function (bool) {
      return A2(boolProperty,
      "seamless",
      bool);
   };
   var checked = function (bool) {
      return A2(boolProperty,
      "checked",
      bool);
   };
   var selected = function (bool) {
      return A2(boolProperty,
      "selected",
      bool);
   };
   var autofocus = function (bool) {
      return A2(boolProperty,
      "autofocus",
      bool);
   };
   var disabled = function (bool) {
      return A2(boolProperty,
      "disabled",
      bool);
   };
   var multiple = function (bool) {
      return A2(boolProperty,
      "multiple",
      bool);
   };
   var novalidate = function (bool) {
      return A2(boolProperty,
      "noValidate",
      bool);
   };
   var readonly = function (bool) {
      return A2(boolProperty,
      "readOnly",
      bool);
   };
   var required = function (bool) {
      return A2(boolProperty,
      "required",
      bool);
   };
   var ismap = function (value) {
      return A2(boolProperty,
      "isMap",
      value);
   };
   var download = function (bool) {
      return A2(boolProperty,
      "download",
      bool);
   };
   var reversed = function (bool) {
      return A2(boolProperty,
      "reversed",
      bool);
   };
   var classList = function (list) {
      return $class($String.join(" ")($List.map($Basics.fst)($List.filter($Basics.snd)(list))));
   };
   var style = function (props) {
      return property("style")($Json$Encode.object($List.map(function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: _v0._0
                      ,_1: $Json$Encode.string(_v0._1)};}
            _U.badCase($moduleName,
            "on line 133, column 35 to 57");
         }();
      })(props)));
   };
   var key = function (k) {
      return A2(stringProperty,
      "key",
      k);
   };
   _elm.Html.Attributes.values = {_op: _op
                                 ,key: key
                                 ,style: style
                                 ,classList: classList
                                 ,property: property
                                 ,stringProperty: stringProperty
                                 ,boolProperty: boolProperty
                                 ,attribute: attribute
                                 ,$class: $class
                                 ,hidden: hidden
                                 ,id: id
                                 ,title: title
                                 ,accesskey: accesskey
                                 ,contenteditable: contenteditable
                                 ,contextmenu: contextmenu
                                 ,dir: dir
                                 ,draggable: draggable
                                 ,dropzone: dropzone
                                 ,itemprop: itemprop
                                 ,lang: lang
                                 ,spellcheck: spellcheck
                                 ,tabindex: tabindex
                                 ,async: async
                                 ,charset: charset
                                 ,content: content
                                 ,defer: defer
                                 ,httpEquiv: httpEquiv
                                 ,language: language
                                 ,scoped: scoped
                                 ,src: src
                                 ,height: height
                                 ,width: width
                                 ,alt: alt
                                 ,autoplay: autoplay
                                 ,controls: controls
                                 ,loop: loop
                                 ,preload: preload
                                 ,poster: poster
                                 ,$default: $default
                                 ,kind: kind
                                 ,srclang: srclang
                                 ,sandbox: sandbox
                                 ,seamless: seamless
                                 ,srcdoc: srcdoc
                                 ,type$: type$
                                 ,value: value
                                 ,checked: checked
                                 ,placeholder: placeholder
                                 ,selected: selected
                                 ,accept: accept
                                 ,acceptCharset: acceptCharset
                                 ,action: action
                                 ,autocomplete: autocomplete
                                 ,autofocus: autofocus
                                 ,autosave: autosave
                                 ,disabled: disabled
                                 ,enctype: enctype
                                 ,formaction: formaction
                                 ,list: list
                                 ,minlength: minlength
                                 ,maxlength: maxlength
                                 ,method: method
                                 ,multiple: multiple
                                 ,name: name
                                 ,novalidate: novalidate
                                 ,pattern: pattern
                                 ,readonly: readonly
                                 ,required: required
                                 ,size: size
                                 ,$for: $for
                                 ,form: form
                                 ,max: max
                                 ,min: min
                                 ,step: step
                                 ,cols: cols
                                 ,rows: rows
                                 ,wrap: wrap
                                 ,ismap: ismap
                                 ,usemap: usemap
                                 ,shape: shape
                                 ,coords: coords
                                 ,challenge: challenge
                                 ,keytype: keytype
                                 ,align: align
                                 ,cite: cite
                                 ,href: href
                                 ,target: target
                                 ,download: download
                                 ,downloadAs: downloadAs
                                 ,hreflang: hreflang
                                 ,media: media
                                 ,ping: ping
                                 ,rel: rel
                                 ,datetime: datetime
                                 ,pubdate: pubdate
                                 ,reversed: reversed
                                 ,start: start
                                 ,colspan: colspan
                                 ,headers: headers
                                 ,rowspan: rowspan
                                 ,scope: scope
                                 ,manifest: manifest};
   return _elm.Html.Attributes.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values)
   return _elm.Html.Events.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Events",
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var keyCode = A2($Json$Decode._op[":="],
   "keyCode",
   $Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"checked"]),
   $Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"value"]),
   $Json$Decode.string);
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,
   addr,
   msg) {
      return A3(on,
      name,
      $Json$Decode.value,
      function (_v0) {
         return function () {
            return A2($Signal.message,
            addr,
            msg);
         }();
      });
   });
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,
   addr,
   handler) {
      return A3(on,
      name,
      keyCode,
      function (code) {
         return A2($Signal.message,
         addr,
         handler(code));
      });
   });
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   _elm.Html.Events.values = {_op: _op
                             ,onBlur: onBlur
                             ,onFocus: onFocus
                             ,onSubmit: onSubmit
                             ,onKeyUp: onKeyUp
                             ,onKeyDown: onKeyDown
                             ,onKeyPress: onKeyPress
                             ,onClick: onClick
                             ,onDoubleClick: onDoubleClick
                             ,onMouseMove: onMouseMove
                             ,onMouseDown: onMouseDown
                             ,onMouseUp: onMouseUp
                             ,onMouseEnter: onMouseEnter
                             ,onMouseLeave: onMouseLeave
                             ,onMouseOver: onMouseOver
                             ,onMouseOut: onMouseOut
                             ,on: on
                             ,targetValue: targetValue
                             ,targetChecked: targetChecked
                             ,keyCode: keyCode};
   return _elm.Html.Events.values;
};
Elm.Inputs = Elm.Inputs || {};
Elm.Inputs.make = function (_elm) {
   "use strict";
   _elm.Inputs = _elm.Inputs || {};
   if (_elm.Inputs.values)
   return _elm.Inputs.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Inputs",
   $GameTypes = Elm.GameTypes.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var backToMenuChan = $Signal.mailbox({ctor: "_Tuple0"});
   var resetLevelChan = $Signal.mailbox({ctor: "_Tuple0"});
   var setEndStateChan = $Signal.mailbox($GameTypes.Normal);
   var startGameChan = $Signal.mailbox({ctor: "_Tuple0"});
   var playLevelOfDifficultyChan = $Signal.mailbox($GameTypes.S);
   var hoverMoveChan = $Signal.mailbox($Maybe.Nothing);
   var clickMoveChan = $Signal.mailbox($Maybe.Nothing);
   var moveClicks = clickMoveChan.signal;
   _elm.Inputs.values = {_op: _op
                        ,moveClicks: moveClicks
                        ,clickMoveChan: clickMoveChan
                        ,hoverMoveChan: hoverMoveChan
                        ,playLevelOfDifficultyChan: playLevelOfDifficultyChan
                        ,startGameChan: startGameChan
                        ,setEndStateChan: setEndStateChan
                        ,resetLevelChan: resetLevelChan
                        ,backToMenuChan: backToMenuChan};
   return _elm.Inputs.values;
};
Elm.Isom = Elm.Isom || {};
Elm.Isom.make = function (_elm) {
   "use strict";
   _elm.Isom = _elm.Isom || {};
   if (_elm.Isom.values)
   return _elm.Isom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Isom",
   $Basics = Elm.Basics.make(_elm),
   $Config = Elm.Config.make(_elm),
   $Easing = Elm.Easing.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm),
   $Util = Elm.Util.make(_elm);
   var firstDo = F2(function (x,
   y) {
      return A2($Transform2D.multiply,
      y,
      x);
   });
   var rationalRot = function (x) {
      return 2 * $Basics.pi * $Ratio.toFloat(x);
   };
   var sInterpret = function (m) {
      return function () {
         switch (m.ctor)
         {case "Identity":
            return $Transform2D.identity;
            case "Reflection":
            return A2($Util.foldr1,
              $Transform2D.multiply,
              _L.fromArray([$Transform2D.rotation(m._0)
                           ,$Transform2D.scaleY(-1)
                           ,$Transform2D.rotation(0 - m._0)]));
            case "Rotation":
            return $Transform2D.rotation(rationalRot(m._0));
            case "Translation":
            return A2($Basics.uncurry,
              $Transform2D.translation,
              m._0);}
         _U.badCase($moduleName,
         "between lines 22 and 31");
      }();
   };
   var interpret = F2(function (t,
   tInit) {
      return $Piece.map(firstDo(tInit))($Piece.$for($Config.transitionTime)(function () {
         switch (t.ctor)
         {case "Identity":
            return function (_v8) {
                 return function () {
                    return $Transform2D.identity;
                 }();
              };
            case "Reflection":
            return function () {
                 var rInv = $Transform2D.rotation(0 - t._0);
                 var r = $Transform2D.rotation(t._0);
                 return function ($) {
                    return function (x) {
                       return A2($Transform2D.multiply,
                       r,
                       A2($Transform2D.multiply,
                       x,
                       rInv));
                    }($Transform2D.scaleY(A5($Easing.ease,
                    $Easing.easeInOutQuad,
                    $Easing.$float,
                    1,
                    -1,
                    $Config.transitionTime)($)));
                 };
              }();
            case "Rotation":
            return function ($) {
                 return $Transform2D.rotation(A5($Easing.ease,
                 $Easing.easeInOutQuad,
                 $Easing.$float,
                 0,
                 rationalRot(t._0),
                 $Config.transitionTime)($));
              };
            case "Translation":
            return function ($) {
                 return $Basics.uncurry($Transform2D.translation)(A5($Easing.ease,
                 $Easing.easeInOutQuad,
                 $Easing.pair($Easing.$float),
                 {ctor: "_Tuple2",_0: 0,_1: 0},
                 t._0,
                 $Config.transitionTime)($));
              };}
         _U.badCase($moduleName,
         "between lines 34 and 51");
      }()));
   });
   var Identity = {ctor: "Identity"};
   var identity = Identity;
   var Reflection = function (a) {
      return {ctor: "Reflection"
             ,_0: a};
   };
   var reflection = function ($) {
      return Reflection($Util.normalizeAngle($));
   };
   var Rotation = function (a) {
      return {ctor: "Rotation"
             ,_0: a};
   };
   var rotation = function ($) {
      return Rotation($Util.normalizeCirculan($));
   };
   var Translation = function (a) {
      return {ctor: "Translation"
             ,_0: a};
   };
   var translation = Translation;
   _elm.Isom.values = {_op: _op
                      ,Translation: Translation
                      ,Rotation: Rotation
                      ,Reflection: Reflection
                      ,Identity: Identity
                      ,rationalRot: rationalRot
                      ,sInterpret: sInterpret
                      ,interpret: interpret
                      ,firstDo: firstDo
                      ,rotation: rotation
                      ,translation: translation
                      ,reflection: reflection
                      ,identity: identity};
   return _elm.Isom.values;
};
Elm.Iterator = Elm.Iterator || {};
Elm.Iterator.make = function (_elm) {
   "use strict";
   _elm.Iterator = _elm.Iterator || {};
   if (_elm.Iterator.values)
   return _elm.Iterator.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Iterator",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Iterator = Elm.Native.Iterator.make(_elm);
   var fold = $Native$Iterator.fold;
   var foldWhile = $Native$Iterator.foldWhile;
   var KeepGoing = function (a) {
      return {ctor: "KeepGoing"
             ,_0: a};
   };
   var Finished = function (a) {
      return {ctor: "Finished"
             ,_0: a};
   };
   var find = function (p) {
      return A2(foldWhile,
      F2(function (x,_v0) {
         return function () {
            return p(x) ? Finished($Maybe.Just(x)) : KeepGoing($Maybe.Nothing);
         }();
      }),
      KeepGoing($Maybe.Nothing));
   };
   var findMany = F2(function (k,
   p) {
      return function ($) {
         return $Basics.fst(A2(foldWhile,
         F2(function (x,_v2) {
            return function () {
               switch (_v2.ctor)
               {case "_Tuple2":
                  return _U.eq(_v2._1,
                    0) ? Finished({ctor: "_Tuple2"
                                  ,_0: _v2._0
                                  ,_1: 0}) : p(x) ? KeepGoing({ctor: "_Tuple2"
                                                              ,_0: A2($List._op["::"],
                                                              x,
                                                              _v2._0)
                                                              ,_1: _v2._1 - 1}) : KeepGoing({ctor: "_Tuple2"
                                                                                            ,_0: _v2._0
                                                                                            ,_1: _v2._1});}
               _U.badCase($moduleName,
               "between lines 114 and 116");
            }();
         }),
         KeepGoing({ctor: "_Tuple2"
                   ,_0: _L.fromArray([])
                   ,_1: k}))($));
      };
   });
   var all = function (p) {
      return A2(foldWhile,
      F2(function (x,_v6) {
         return function () {
            return p(x) ? KeepGoing(true) : Finished(false);
         }();
      }),
      KeepGoing(true));
   };
   var and = all($Basics.identity);
   var Cat = F2(function (a,b) {
      return {ctor: "Cat"
             ,_0: a
             ,_1: b};
   });
   var concat = function (t) {
      return function () {
         switch (t.ctor)
         {case "Cat": return A2(Cat,
              t._0,
              function ($) {
                 return concat(t._1($));
              });
            case "Fun": return A2(Cat,
              t._0,
              t._1);}
         _U.badCase($moduleName,
         "between lines 73 and 79");
      }();
   };
   var Fun = F2(function (a,b) {
      return {ctor: "Fun"
             ,_0: a
             ,_1: b};
   });
   var map = F2(function (f,t) {
      return function () {
         switch (t.ctor)
         {case "Cat": return A2(Cat,
              t._0,
              function ($) {
                 return map(f)(t._1($));
              });
            case "Fun": return A2(Fun,
              t._0,
              function ($) {
                 return f(t._1($));
              });}
         _U.badCase($moduleName,
         "between lines 43 and 45");
      }();
   });
   var concatMap = function (f) {
      return function ($) {
         return concat(map(f)($));
      };
   };
   var indexedMap = F2(function (f,
   t) {
      return function () {
         switch (t.ctor)
         {case "Cat": return A2(Cat,
              t._0,
              function ($) {
                 return indexedMap(f)(t._1($));
              });
            case "Fun": return A2(Fun,
              t._0,
              function (i) {
                 return A2(f,i,t._1(i));
              });}
         _U.badCase($moduleName,
         "between lines 48 and 54");
      }();
   });
   var upTil = Fun;
   var range = F2(function (start,
   stop) {
      return A2(upTil,
      stop - start + 1,
      function (i) {
         return start + i;
      });
   });
   var fromArray = function (a) {
      return function () {
         var n = $Array.length(a);
         return A2(upTil,
         n,
         function (i) {
            return function () {
               var _v23 = A2($Array.get,
               i,
               a);
               switch (_v23.ctor)
               {case "Just": return _v23._0;}
               _U.badCase($moduleName,
               "on line 69, column 18 to 51");
            }();
         });
      }();
   };
   var fromList = function ($) {
      return fromArray($Array.fromList($));
   };
   _elm.Iterator.values = {_op: _op
                          ,map: map
                          ,upTil: upTil
                          ,range: range
                          ,concat: concat
                          ,concatMap: concatMap
                          ,fromList: fromList
                          ,fromArray: fromArray
                          ,indexedMap: indexedMap
                          ,foldWhile: foldWhile
                          ,fold: fold
                          ,find: find
                          ,findMany: findMany
                          ,all: all
                          ,and: and
                          ,Finished: Finished
                          ,KeepGoing: KeepGoing};
   return _elm.Iterator.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values)
   return _elm.Json.Decode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Decode",
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,
   decoder) {
      return A3($List.foldr,
      F2(function (x,y) {
         return A2(_op[":="],x,y);
      }),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,
      $Dict.fromList,
      keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   _elm.Json.Decode.values = {_op: _op
                             ,Decoder: Decoder
                             ,map: map
                             ,decodeString: decodeString
                             ,at: at
                             ,object1: object1
                             ,object2: object2
                             ,object3: object3
                             ,object4: object4
                             ,object5: object5
                             ,object6: object6
                             ,object7: object7
                             ,object8: object8
                             ,keyValuePairs: keyValuePairs
                             ,dict: dict
                             ,oneOf: oneOf
                             ,string: string
                             ,$float: $float
                             ,$int: $int
                             ,bool: bool
                             ,list: list
                             ,array: array
                             ,$null: $null
                             ,maybe: maybe
                             ,value: value
                             ,decodeValue: decodeValue
                             ,customDecoder: customDecoder
                             ,andThen: andThen
                             ,fail: fail
                             ,succeed: succeed
                             ,tuple1: tuple1
                             ,tuple2: tuple2
                             ,tuple3: tuple3
                             ,tuple4: tuple4
                             ,tuple5: tuple5
                             ,tuple6: tuple6
                             ,tuple7: tuple7
                             ,tuple8: tuple8};
   return _elm.Json.Decode.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values)
   return _elm.Json.Encode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Encode",
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   _elm.Json.Encode.values = {_op: _op
                             ,Value: Value
                             ,encode: encode
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,object: object
                             ,array: array
                             ,list: list};
   return _elm.Json.Encode.values;
};
Elm.Level = Elm.Level || {};
Elm.Level.make = function (_elm) {
   "use strict";
   _elm.Level = _elm.Level || {};
   if (_elm.Level.values)
   return _elm.Level.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Level",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Config = Elm.Config.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Draw = Elm.Draw.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Generate = Elm.Generate.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Inputs = Elm.Inputs.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Signal$Extra = Elm.Signal.Extra.make(_elm),
   $Style = Elm.Style.make(_elm),
   $Time = Elm.Time.make(_elm),
   $Util = Elm.Util.make(_elm),
   $Window = Elm.Window.make(_elm);
   var sendSets = F2(function (setLocalStorageChan,
   state) {
      return $Signal.map($Signal.send(setLocalStorageChan))($Signal.dropRepeats(A3($Signal$Extra.foldp$,
      $Basics.max,
      $Basics.identity,
      A2($Signal.map,
      function (_) {
         return _.totalScore;
      },
      state))));
   });
   var bordered = F3(function (r,
   c,
   elt) {
      return A2($Graphics$Element.color,
      c,
      A4($Graphics$Element.container,
      $Graphics$Element.widthOf(elt) + 2 * r,
      $Graphics$Element.heightOf(elt) + 2 * r,
      $Graphics$Element.middle,
      elt));
   });
   var pad = F2(function (k,e) {
      return A4($Graphics$Element.container,
      $Graphics$Element.widthOf(e) + 2 * k,
      $Graphics$Element.heightOf(e) + 2 * k,
      $Graphics$Element.middle,
      e);
   });
   var wrapWithClass = F2(function (c,
   elt) {
      return A3($Html.toElement,
      $Graphics$Element.widthOf(elt),
      $Graphics$Element.heightOf(elt),
      A2($Html.div,
      _L.fromArray([$Html$Attributes.$class(c)]),
      _L.fromArray([$Html.fromElement(elt)])));
   });
   var startTime = A2($Signal.sampleOn,
   $Signal.constant({ctor: "_Tuple0"}),
   $Time.every(1000));
   var initialLevelState = function (lev) {
      return {_: {}
             ,endState: $GameTypes.Normal
             ,movesLeft: lev.maxMoves
             ,postMove: lev.initial
             ,preMove: lev.initial};
   };
   var playLevelOfDifficulty = F2(function (d,
   s) {
      return function () {
         var $ = A2($Random.generate,
         $Generate.randomLevel(d),
         s.seed),
         lev = $._0,
         seed$ = $._1;
         return _U.replace([["levelState"
                            ,initialLevelState(lev)]
                           ,["currLevel",lev]
                           ,["lastMove",$Maybe.Nothing]
                           ,["seed",seed$]],
         s);
      }();
   });
   var updateWithMove = F2(function (m,
   s) {
      return function () {
         var ls = s.levelState;
         var postMove$ = A2($Move.sMultiply,
         $Move.sInterpret(m),
         ls.postMove);
         var $ = function () {
            var _v0 = ls.endState;
            switch (_v0.ctor)
            {case "End":
               switch (_v0._1.ctor)
                 {case "Havent":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["endState"
                                            ,A2($GameTypes.End,
                                            _v0._0,
                                            $GameTypes.Have)]],
                           ls)
                           ,_1: 0};}
                 switch (_v0._0.ctor)
                 {case "Win":
                    switch (_v0._1.ctor)
                      {case "Have":
                         return {ctor: "_Tuple2"
                                ,_0: ls
                                ,_1: 0};}
                      break;}
                 break;}
            return function () {
               var scoreIncr = $GameTypes.difficultyScore(s.currLevel.difficulty);
               var totalScore$ = s.totalScore + scoreIncr;
               return $Util.allTogether(postMove$) ? function () {
                  var movesLeft = ls.movesLeft - 1;
                  return {ctor: "_Tuple2"
                         ,_0: {_: {}
                              ,endState: A2($GameTypes.End,
                              $GameTypes.Win({_: {}
                                             ,difficulty: s.currLevel.difficulty
                                             ,move: m
                                             ,movesLeft: movesLeft
                                             ,pre: ls.postMove
                                             ,totalScore: totalScore$}),
                              $GameTypes.Havent)
                              ,movesLeft: movesLeft
                              ,postMove: postMove$
                              ,preMove: ls.postMove}
                         ,_1: scoreIncr};
               }() : _U.eq(ls.movesLeft,
               1) ? function () {
                  var lose = {_: {}
                             ,init: s.currLevel.initial
                             ,maxMoves: s.currLevel.maxMoves
                             ,move: m
                             ,pre: ls.postMove};
                  var s0 = initialLevelState(s.currLevel);
                  return {ctor: "_Tuple2"
                         ,_0: _U.replace([["endState"
                                          ,A2($GameTypes.End,
                                          $GameTypes.Lose(lose),
                                          $GameTypes.Havent)]],
                         s0)
                         ,_1: 0};
               }() : {ctor: "_Tuple2"
                     ,_0: {_: {}
                          ,endState: $GameTypes.Normal
                          ,movesLeft: ls.movesLeft - 1
                          ,postMove: postMove$
                          ,preMove: ls.postMove}
                     ,_1: 0};
            }();
         }(),
         ls$ = $._0,
         scoreIncr = $._1;
         return _U.replace([["levelState"
                            ,ls$]
                           ,["lastMove",$Maybe.Just(m)]
                           ,["totalScore"
                            ,s.totalScore + scoreIncr]],
         s);
      }();
   });
   var update = F2(function (u,s) {
      return function () {
         var _v4 = A2($Debug.log,
         "update:",
         u);
         switch (_v4.ctor)
         {case "Clicked":
            return A2(updateWithMove,
              _v4._0,
              s);
            case "Hovered": return s;
            case "NoOp": return s;
            case "PlayLevelOfDifficulty":
            return A2(playLevelOfDifficulty,
              _v4._0,
              s);
            case "ResetLevel":
            return _U.replace([["levelState"
                               ,initialLevelState(s.currLevel)]],
              s);
            case "SetEndState":
            return function () {
                 var ls = s.levelState;
                 return _U.replace([["levelState"
                                    ,_U.replace([["endState"
                                                 ,_v4._0]],
                                    ls)]],
                 s);
              }();
            case "SetTotalScore":
            return _U.replace([["totalScore"
                               ,_v4._0]],
              s);
            case "Unhovered": return s;}
         _U.badCase($moduleName,
         "between lines 53 and 61");
      }();
   });
   var initialGameState = F2(function (seed,
   lev) {
      return {_: {}
             ,currLevel: lev
             ,lastMove: $Maybe.Nothing
             ,levelState: initialLevelState(lev)
             ,seed: seed
             ,totalScore: 0};
   });
   var run = F2(function (setTotalScore,
   setLocalStorageChan) {
      return function () {
         var gameMode = A3($Signal.foldp,
         F2(function (_v10,_v11) {
            return function () {
               return function () {
                  return $GameTypes.PlayLevel;
               }();
            }();
         }),
         $GameTypes.TitleScreen,
         $Inputs.startGameChan.signal);
         var openingScreen = $Draw.chooseDifficultyScreen(0);
         var updates = $Signal.mergeMany(_L.fromArray([A3($Util.filterMap,
                                                      $Maybe.map($GameTypes.Clicked),
                                                      $GameTypes.NoOp,
                                                      $Inputs.clickMoveChan.signal)
                                                      ,A2($Signal.map,
                                                      $GameTypes.PlayLevelOfDifficulty,
                                                      $Inputs.playLevelOfDifficultyChan.signal)
                                                      ,A2($Signal.map,
                                                      $GameTypes.SetEndState,
                                                      $Inputs.setEndStateChan.signal)
                                                      ,A2($Signal.map,
                                                      function (_v14) {
                                                         return function () {
                                                            return $GameTypes.ResetLevel;
                                                         }();
                                                      },
                                                      $Inputs.resetLevelChan.signal)
                                                      ,A2($Signal.map,
                                                      $GameTypes.SetTotalScore,
                                                      setTotalScore)]));
         var dummyLevel = {_: {}
                          ,availableMoves: _L.fromArray([])
                          ,difficulty: $GameTypes.S
                          ,initial: _L.fromArray([])
                          ,maxMoves: 0};
         var state = A3($Signal$Extra.foldp$,
         function (_v16) {
            return function () {
               switch (_v16.ctor)
               {case "_Tuple2":
                  return update(_v16._1);}
               _U.badCase($moduleName,
               "on line 136, column 35 to 43");
            }();
         },
         function (_v20) {
            return function () {
               switch (_v20.ctor)
               {case "_Tuple2":
                  return A2(initialGameState,
                    $Random.initialSeed($Basics.floor(_v20._0)),
                    dummyLevel);}
               _U.badCase($moduleName,
               "on line 137, column 24 to 83");
            }();
         },
         A3($Signal.map2,
         F2(function (v0,v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }),
         startTime,
         updates));
         var stages = A3($Draw.animations,
         openingScreen,
         updates,
         state);
         var buttons = A2($Signal.map,
         function ($) {
            return $Graphics$Element.color($Color.white)(A2($Html.toElement,
            $Config.w,
            $Config.buttonsHeight)($Draw.transButtons(function (_) {
               return _.currLevel;
            }($))));
         },
         state);
         var hovers = A2($Signal.map2,
         F2(function (s,x) {
            return function () {
               var _v26 = s.levelState.endState;
               switch (_v26.ctor)
               {case "Normal": return x;}
               return $Maybe.Nothing;
            }();
         }),
         state)($Signal.merge(A2($Signal.map,
         function (_v24) {
            return function () {
               return $Maybe.Nothing;
            }();
         },
         $Inputs.clickMoveChan.signal))($Signal.dropRepeats($Inputs.hoverMoveChan.signal)));
         var ends_ = $Draw.loseAnimEnds(state);
         var sets_ = A2(sendSets,
         setLocalStorageChan,
         state);
         var hoverOverlay = A3($Signal.map2,
         F2(function (mm,s) {
            return A3($Util.maybe,
            $Graphics$Element.empty,
            $Draw.hoverArt(s.levelState),
            mm);
         }),
         hovers,
         state);
         return {ctor: "_Tuple3"
                ,_0: ends_
                ,_1: sets_
                ,_2: A7($Util.signalMap6,
                F6(function (mode,
                mainScreen,
                hov,
                butts,
                s,
                _v27) {
                   return function () {
                      switch (_v27.ctor)
                      {case "_Tuple2":
                         return function () {
                              var screen = function () {
                                 switch (mode.ctor)
                                 {case "ChooseLevel":
                                    return $Draw.chooseDifficultyScreen(s.totalScore);
                                    case "PlayLevel":
                                    return A2($Graphics$Element.flow,
                                      $Graphics$Element.inward,
                                      _L.fromArray([function () {
                                                      var dbs = $Draw.difficultyButtonsOverlay;
                                                      return A4($Graphics$Element.container,
                                                      $Config.w,
                                                      $Graphics$Element.heightOf(dbs),
                                                      A2($Graphics$Element.topRightAt,
                                                      $Graphics$Element.absolute(0),
                                                      $Graphics$Element.absolute(3)),
                                                      dbs);
                                                   }()
                                                   ,A2($Graphics$Element.flow,
                                                   $Graphics$Element.down,
                                                   _L.fromArray([A2($Graphics$Element.flow,
                                                                $Graphics$Element.inward,
                                                                _L.fromArray([hov
                                                                             ,mainScreen
                                                                             ,A3($Graphics$Collage.collage,
                                                                             $Config.w,
                                                                             $Config.h,
                                                                             _L.fromArray([A2($Graphics$Collage.filled,
                                                                             $Color.white,
                                                                             A2($Graphics$Collage.rect,
                                                                             $Config.w,
                                                                             $Config.h))]))]))
                                                                ,butts]))]));
                                    case "TitleScreen":
                                    return $Draw.titleScreen;}
                                 _U.badCase($moduleName,
                                 "between lines 170 and 187");
                              }();
                              var game = A4($Graphics$Element.container,
                              _v27._0,
                              $Config.totalHeight,
                              $Graphics$Element.middle,
                              screen);
                              return A2($Graphics$Element.flow,
                              $Graphics$Element.inward,
                              _L.fromArray([$Style.globalStyle
                                           ,game]));
                           }();}
                      _U.badCase($moduleName,
                      "between lines 169 and 192");
                   }();
                }),
                gameMode,
                A2($Piece.run,
                $Time.every(30),
                stages),
                hoverOverlay,
                buttons,
                state,
                $Window.dimensions)};
      }();
   });
   _elm.Level.values = {_op: _op
                       ,initialGameState: initialGameState
                       ,initialLevelState: initialLevelState
                       ,update: update
                       ,playLevelOfDifficulty: playLevelOfDifficulty
                       ,updateWithMove: updateWithMove
                       ,run: run
                       ,startTime: startTime
                       ,wrapWithClass: wrapWithClass
                       ,pad: pad
                       ,bordered: bordered
                       ,sendSets: sendSets};
   return _elm.Level.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,
      $Basics.identity,
      xs);
   };
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = $Native$List.any;
   var all = F2(function (pred,
   xs) {
      return $Basics.not(A2(any,
      function ($) {
         return $Basics.not(pred($));
      },
      xs));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_v0,i) {
         return function () {
            return i + 1;
         }();
      }),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var maximum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.max,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var minimum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.min,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var member = F2(function (x,
   xs) {
      return A2(any,
      function (a) {
         return _U.eq(a,x);
      },
      xs);
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._1);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var head = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 75 and 84");
      }();
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {
         return A2(_op["::"],
         f(x),
         acc);
      }),
      _L.fromArray([]),
      xs);
   });
   var filter = F2(function (pred,
   xs) {
      return function () {
         var conditionalCons = F2(function (x,
         xs$) {
            return pred(x) ? A2(_op["::"],
            x,
            xs$) : xs$;
         });
         return A3(foldr,
         conditionalCons,
         _L.fromArray([]),
         xs);
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v15 = f(mx);
         switch (_v15.ctor)
         {case "Just":
            return A2(_op["::"],_v15._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 179 and 186");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {
         return A2(_op["::"],x,y);
      }),
      _L.fromArray([]),
      list);
   };
   var scanl = F3(function (f,
   b,
   xs) {
      return function () {
         var scan1 = F2(function (x,
         accAcc) {
            return function () {
               switch (accAcc.ctor)
               {case "::": return A2(_op["::"],
                    A2(f,x,accAcc._0),
                    accAcc);
                  case "[]":
                  return _L.fromArray([]);}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         });
         return reverse(A3(foldl,
         scan1,
         _L.fromArray([b]),
         xs));
      }();
   });
   var append = F2(function (xs,
   ys) {
      return function () {
         switch (ys.ctor)
         {case "[]": return xs;}
         return A3(foldr,
         F2(function (x,y) {
            return A2(_op["::"],x,y);
         }),
         ys,
         xs);
      }();
   });
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v21._0)
                                   ,_1: _v21._1} : {ctor: "_Tuple2"
                                                   ,_0: _v21._0
                                                   ,_1: A2(_op["::"],
                                                   x,
                                                   _v21._1)};}
               _U.badCase($moduleName,
               "between lines 301 and 303");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v25,
         _v26) {
            return function () {
               switch (_v26.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v25.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],
                                 _v25._0,
                                 _v26._0)
                                 ,_1: A2(_op["::"],
                                 _v25._1,
                                 _v26._1)};}
                       _U.badCase($moduleName,
                       "on line 339, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 339, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 350 and 361");
      }();
   });
   _elm.List.values = {_op: _op
                      ,isEmpty: isEmpty
                      ,length: length
                      ,reverse: reverse
                      ,member: member
                      ,head: head
                      ,tail: tail
                      ,filter: filter
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,append: append
                      ,concat: concat
                      ,intersperse: intersperse
                      ,partition: partition
                      ,unzip: unzip
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,filterMap: filterMap
                      ,concatMap: concatMap
                      ,indexedMap: indexedMap
                      ,foldr: foldr
                      ,foldl: foldl
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,all: all
                      ,any: any
                      ,scanl: scanl
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main",
   $Basics = Elm.Basics.make(_elm),
   $Isom = Elm.Isom.make(_elm),
   $Level = Elm.Level.make(_elm),
   $List = Elm.List.make(_elm),
   $Move = Elm.Move.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm),
   $Util = Elm.Util.make(_elm);
   var setTotalScore = Elm.Native.Port.make(_elm).inboundSignal("setTotalScore",
   "Int",
   function (v) {
      return typeof v === "number" ? v : _U.badPort("a number",
      v);
   });
   var setLocalStorageChan = $Signal.mailbox(0);
   var setLocalStorage = Elm.Native.Port.make(_elm).outboundSignal("setLocalStorage",
   function (v) {
      return v;
   },
   setLocalStorageChan.signal);
   var $ = A2($Level.run,
   setTotalScore,
   setLocalStorageChan.address),
   ends_ = $._0,
   sets_ = $._1,
   game = $._2;
   var ends = Elm.Native.Task.make(_elm).performSignal("ends",
   ends_);
   var sets = Elm.Native.Task.make(_elm).performSignal("sets",
   sets_);
   var main = game;
   _op["<>"] = $Transform2D.multiply;
   var invMove = function ($) {
      return $List.map($Util.invert)($Move.sInterpret($));
   };
   var salted = function (salt) {
      return function ($) {
         return $List.map(function (m) {
            return A2($Transform2D.multiply,
            m,
            salt);
         })($Util.foldr1($Move.sMultiply)($List.map(invMove)($List.reverse($))));
      };
   };
   var veryEasy1 = function () {
      var m1 = _L.fromArray([$Isom.translation({ctor: "_Tuple2"
                                               ,_0: 0
                                               ,_1: -100})
                            ,$Isom.identity]);
      var m0 = _L.fromArray([$Isom.reflection($Basics.pi / 2)
                            ,$Isom.identity]);
      return {_: {}
             ,availableMoves: _L.fromArray([m0
                                           ,m1])
             ,initial: A2(salted,
             A2($Transform2D.translation,
             -100,
             0),
             _L.fromArray([m0,m1]))
             ,maxMoves: 2};
   }();
   var reflection = function (a) {
      return A2($Transform2D.multiply,
      $Transform2D.rotation(a),
      A2($Transform2D.multiply,
      $Transform2D.scaleY(-1),
      $Transform2D.rotation(0 - a)));
   };
   var easy1 = function () {
      var m2 = _L.fromArray([$Isom.reflection(0)
                            ,$Isom.rotation(A2($Ratio.over,
                            2,
                            3))]);
      var m1 = _L.fromArray([$Isom.translation({ctor: "_Tuple2"
                                               ,_0: -100
                                               ,_1: 0})
                            ,$Isom.rotation(A2($Ratio.over,
                            2,
                            3))]);
      var m0 = _L.fromArray([$Isom.reflection(2 * $Basics.pi / 3)
                            ,$Isom.rotation(A2($Ratio.over,
                            2,
                            3))]);
      return {_: {}
             ,availableMoves: _L.fromArray([m0
                                           ,m1
                                           ,m2])
             ,initial: A2(salted,
             A2(_op["<>"],
             A2(_op["<>"],
             A2($Transform2D.translation,
             -50,
             50),
             $Transform2D.rotation($Basics.pi / 3)),
             reflection($Basics.pi / 2)),
             _L.fromArray([m2,m0,m1]))
             ,maxMoves: 3};
   }();
   var veryEasy2 = function () {
      var m1 = _L.fromArray([$Isom.identity
                            ,$Isom.rotation(A2($Ratio.over,
                            2,
                            3))]);
      var m0 = _L.fromArray([$Isom.reflection($Basics.pi / 2)
                            ,$Isom.identity]);
      return {_: {}
             ,availableMoves: _L.fromArray([m0
                                           ,m1])
             ,initial: A2(salted,
             A2(_op["<>"],
             A2($Transform2D.translation,
             -100,
             100),
             reflection($Basics.pi / 2)),
             _L.fromArray([m0,m1]))
             ,maxMoves: 2};
   }();
   _elm.Main.values = {_op: _op
                      ,reflection: reflection
                      ,salted: salted
                      ,invMove: invMove
                      ,easy1: easy1
                      ,veryEasy1: veryEasy1
                      ,veryEasy2: veryEasy2
                      ,ends_: ends_
                      ,game: game
                      ,sets_: sets_
                      ,main: main
                      ,setLocalStorageChan: setLocalStorageChan};
   return _elm.Main.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 56");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 73");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 73");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 107");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Move = Elm.Move || {};
Elm.Move.make = function (_elm) {
   "use strict";
   _elm.Move = _elm.Move || {};
   if (_elm.Move.values)
   return _elm.Move.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Move",
   $Basics = Elm.Basics.make(_elm),
   $Isom = Elm.Isom.make(_elm),
   $List = Elm.List.make(_elm),
   $Piece = Elm.Piece.make(_elm),
   $PieceUtils = Elm.PieceUtils.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var sMultiply = $List.map2($Transform2D.multiply);
   var sInterpret = $List.map($Isom.sInterpret);
   var interpret = F2(function (m,
   mInit) {
      return $PieceUtils.sequence(A2($List.map2,
      F2(function (x,y) {
         return y(x);
      }),
      mInit)(A2($List.map,
      $Isom.interpret,
      m)));
   });
   _elm.Move.values = {_op: _op
                      ,interpret: interpret
                      ,sInterpret: sInterpret
                      ,sMultiply: sMultiply};
   return _elm.Move.values;
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: "_Array",
		height: 0,
		table: new Array()
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				"Index " + i + " is out of range. Check the length of " +
				"your array first or use getMaybe or getWithDefault.");
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height == 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len == 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h == 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: "_Array",
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list == List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = new Array();
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i == M)
			{
				var leaf = {
					ctor: "_Array",
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: "_Array",
				height: 0,
				table: table.splice(0,i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length == 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length == h)
		{
			var node = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length == M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height == 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: "_Array",
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed != null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height == 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? 0 : a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to == length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right == 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from == 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left == a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a,b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a,b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node)
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index == 0 || index == a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: "_Array",
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M == 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length)

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length == M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1,0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height == 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: "_Array",
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h == tree.height)
		{
			return tree;
		}

		return {
			ctor: "_Array",
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: "_Array",
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height == 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t == 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length == 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray:toJSArray,
		fromJSArray:fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;

}

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a/b)|0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error("Cannot perform mod 0. Division by zero error.");
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a,b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a,b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity
	}

	function truncate(n)
	{
		return n|0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees:  degrees,
		turns:  turns,
		fromPolar:  fromPolar,
		toPolar:  toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode : function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode   : function(c) { return c.charCodeAt(0); },
		toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
		toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Show.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary:F3(watchSummary),
	};
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx,style,path)
	{
		(style.dashing.ctor === '[]')
			? trace(ctx, path)
			: customLineHelp(ctx, style, path);
		ctx.scale(1,-1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0,y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(ctx, style);
		ctx.scale(1,-1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0,-2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w/2,
			destY = -h/2,
			destW = w,
			destH = h;

		ctx.scale(1,-1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta);
		}
		if (scale !== 1)
		{
			ctx.scale(scale,scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i].form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i];
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w,h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = "block";
		canvas.style.position = "absolute";
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w,h,div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w,h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w,h,div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w,h,forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w:w, h:h, forms:forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};

};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	function createNode(elementType)
	{
		var node = document.createElement(elementType);
		node.style.padding = "0";
		node.style.margin = "0";
		return node;
	}


	function newElement(width, height, elementPrim)
	{
		return {
			_: {},
			element: elementPrim,
			props: {
				_: {},
				id: Utils.guid(),
				width: width,
				height: height,
				opacity: 1,
				color: Maybe.Nothing,
				href: "",
				tag: "",
				hover: Utils.Tuple0,
				click: Utils.Tuple0
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width |0) + 'px';
		node.style.height = (height|0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img,props.width,props.height,img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = "block";
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = "hidden";

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw)|0) + 'px';
			img.style.height = ((this.height * sh)|0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
			img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp    : goDown,
		DDown  : goDown,
		DLeft  : goRight,
		DRight : goRight,
		DIn    : goOut,
		DOut   : goOut
	};
	function needsReversal(dir)
	{
		return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
	}

	function flow(dir,elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir == goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === "Absolute"
			? pos._0 + "px"
			: (pos._0 * 100) + "%";
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos,elem,e)
	{
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w/2)|0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h/2)|0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos,elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var guid = elem.guid;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = "hidden";
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(elem)
	{
		return setProps(elem, makeElement(elem));
	}
	function makeElement(e)
	{
		var elem = e.element;
		switch(elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, curr, next)
	{
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(next);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch(nextE.ctor)
		{
			case "Spacer":
				updateProps(node, curr, next);
				return rootNode;

			case "RawHtml":
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Image":
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE,currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(next);
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Flow":
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i].element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(next);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(next);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i-- ;)
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Container":
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case "Custom":
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(next);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width|0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height|0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor :'RawHtml',
				html : Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		}
	}

	function markdown(text)
	{
		var raw = {
			ctor:'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	function htmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + "px";
		}
		temp.style.visibility = "hidden";
		temp.style.styleFloat = "left";
		temp.style.cssFloat   = "left";

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
		var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w,h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};

};

Elm.Native.IsomUtil = {};
Elm.Native.IsomUtil.make = function(localRunTime) {
  localRunTime.Native = localRunTime.Native || {};
  localRunTime.Native.IsomUtil = localRunTime.Native.IsomUtil || {};

  var Utils = Elm.Native.Utils.make(localRunTime);

  if (localRunTime.Native.IsomUtil.values) {
    return localRunTime.Native.IsomUtil.values;
  }

  function stringify(x) {
    return JSON.stringify(x);
  }

  function tuply(x) {
    return {
      ctor : "_Tuple6",
      _0 : x[0],
      _1: x[1],
      _2 : x[2],
      _3 : x[3],
      _4 : x[4],
      _5 : x[5]
    };
  }

  return localRunTime.Native.IsomUtil.values = {
    stringify : stringify,
    tuply     : tuply
  }
};

Elm.Native.Iterator = {};
Elm.Native.Iterator.make = function(localRunTime) {
  localRunTime.Native = localRunTime.Native || {};
  localRunTime.Native.Iterator = localRunTime.Native.Iterator || {};

  var Utils = Elm.Native.Utils.make(localRunTime);

  function foldFun(f, init, g, len) {
    for (var i = 0; i < len; ++i) {
        init = A2(f, g(i), init);
    }
    return init;
  }

  function fold(f, init, t) {
    switch (t.ctor) {
      case "Fun":
        return foldFun(f, init, t._1, t._0);
      default:
        return foldFun(F2(function(tt, acc) { return fold(f, acc, tt); })
                , init, t._1, t._0);
    }
  }

  function foldWhileFunH(f, init, g, len) {
    for (var i = 0; i < len; ++i) {
      switch (init.ctor) {
        case "Finished":
          return init;
        default:
          init = A2(f, g(i), init._0);
      }
    }
    return init;
  }

  function foldWhileH(f, init, t) {
    switch (t.ctor) {
      case "Fun":
        return foldWhileFunH(f, init, t._1, t._0);
      default:
        return foldWhileFunH(F2(function(tt, acc) { return foldWhileH(f, {ctor:'KeepGoing', _0:acc}, tt); }) // argument order
                , init, t._1, t._0);
    }
  }

  function foldWhile(f, init, t) {
    return foldWhileH(f, init, t)._0;
  }

  if (localRunTime.Native.Iterator.values) {
    return localRunTime.Native.Iterator.values;
  }

  return localRunTime.Native.Iterator.values = {
    foldWhile : F3(foldWhile),
    fold : F3(fold)
  }
};

Elm.Native.Json = {};
Elm.Native.Json.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value === 'number' && (value|0) === value) {
			return value;
		}
		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i-- ; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i-- ; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value) {
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash("an object", value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		}
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		}
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		}
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		}
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		}
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};

};

Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo,hi)
	{
		var lst = Nil;
		if (lo <= hi)
		{
			do { lst = Cons(hi,lst) } while (hi-->lo);
		}
		return lst
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function any(pred, xs)
	{
		while (xs.ctor !== '[]')
		{
			if (pred(xs._0))
			{
				return true;
			}
			xs = xs._1;
		}
		return false;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}

	function drop(n, xs)
	{
		while (xs.ctor !== '[]' && n > 0)
		{
			xs = xs._1;
			--n;
		}
		return xs;
	}

	function repeat(n, x)
	{
		var arr = [];
		var pattern = [x];
		while (n > 0)
		{
			if (n & 1)
			{
				arr = arr.concat(pattern);
			}
			n >>= 1, pattern = pattern.concat(pattern);
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil:Nil,
		Cons:Cons,
		cons:F2(Cons),
		toArray:toArray,
		fromArray:fromArray,
		range:range,

		foldl:F3(foldl),
		foldr:F3(foldr),

		any:F2(any),
		map2:F3(map2),
		map3:F4(map3),
		map4:F5(map4),
		map5:F6(map5),
		sortBy:F2(sortBy),
		sortWith:F2(sortWith),
		take:F2(take),
		drop:F2(drop),
		repeat:F2(repeat)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;

};

Elm.Native.Port = {};
Elm.Native.Port.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				"Port Error:\n" +
				"No argument was given for the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You need to provide an initial value!\n\n" +
				"Find out more about ports here <http://elm-lang.org/learn/Ports.elm>"
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				"Port Error:\n" +
				"Regarding the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You just sent the value:\n\n" +
				"    " + JSON.stringify(arg.value) + "\n\n" +
				"but it cannot be converted to the necessary type.\n" +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};


if (!Elm.fullscreen) {

	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];
				// elm.swap = newElm.swap;
				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild == 'undefined')
				{
					console.log(error.message);
				}
				else
				{
					container.appendChild(errorNode(error.message));
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		};

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode("Open the developer console for more details."));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.props)
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000/60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							"Unexpected draw callback.\n" +
							"Please report this to <https://github.com/elm-lang/core/issues>."
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld,nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld,nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b) } }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a,b,c) }}
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a,b,c,d) }}}
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a,b,c,d,e,f) }}}}}
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a,b,c,d,e,f,g,h)}}}}}}}
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun,a,b)
	{
		return fun.arity === 2
			? fun.func(a,b)
			: fun(a)(b);
	}
	function A3(fun,a,b,c)
	{
		return fun.arity === 3
			? fun.func(a,b,c)
			: fun(a)(b)(c);
	}
	function A4(fun,a,b,c,d)
	{
		return fun.arity === 4
			? fun.func(a,b,c,d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun,a,b,c,d,e)
	{
		return fun.arity === 5
			? fun.func(a,b,c,d,e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun,a,b,c,d,e,f)
	{
		return fun.arity === 6
			? fun.func(a,b,c,d,e,f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun,a,b,c,d,e,f,g)
	{
		return fun.arity === 7
			? fun.func(a,b,c,d,e,f,g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun,a,b,c,d,e,f,g,h)
	{
		return fun.arity === 8
			? fun.func(a,b,c,d,e,f,g,h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun,a,b,c,d,e,f,g,h,i)
	{
		return fun.arity === 9
			? fun.func(a,b,c,d,e,f,g,h,i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native.Show = {};
Elm.Native.Show.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Show = localRuntime.Native.Show || {};
	if (localRuntime.Native.Show.values)
	{
		return localRuntime.Native.Show.values;
	}

	var _Array;
	var Dict;
	var List;
	var Utils = Elm.Native.Utils.make(localRuntime);

	var toString = function(v)
	{
		var type = typeof v;
		if (type === "function")
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === "boolean")
		{
			return v ? "True" : "False";
		}
		else if (type === "number")
		{
			return v + "";
		}
		else if ((v instanceof String) && v.isChar)
		{
			return "'" + addSlashes(v, true) + "'";
		}
		else if (type === "string")
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === "object" && '_' in v && probablyPublic(v))
		{
			var output = [];
			for (var k in v._)
			{
				for (var i = v._[k].length; i--; )
				{
					output.push(k + " = " + toString(v._[k][i]));
				}
			}
			for (var k in v)
			{
				if (k === '_') continue;
				output.push(k + " = " + toString(v[k]));
			}
			if (output.length === 0)
			{
				return "{}";
			}
			return "{ " + output.join(", ") + " }";
		}
		else if (type === "object" && 'ctor' in v)
		{
			if (v.ctor.substring(0,6) === "_Tuple")
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return "(" + output.join(",") + ")";
			}
			else if (v.ctor === "_Array")
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return "Array.fromList " + toString(list);
			}
			else if (v.ctor === "::")
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === "::")
				{
					output += "," + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === "[]")
			{
				return "[]";
			}
			else if (v.ctor === "RBNode" || v.ctor === "RBEmpty")
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				if (!List)
				{
					List = Elm.List.make(localRuntime);
				}
				var list = Dict.toList(v);
				var name = "Dict";
				if (list.ctor === "::" && list._0._1.ctor === "_Tuple0")
				{
					name = "Set";
					list = A2(List.map, function(x){return x._0}, list);
				}
				return name + ".fromList " + toString(list);
			}
			else if (v.ctor.slice(0,5) === "Text:")
			{
				return '<text>'
			}
			else
			{
				var output = "";
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return 'initialValue' in v
				? '<Signal>'
				: '<Stream>';
		}
		return "<internal structure>";
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, "\\'")
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}

	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v)
		{
			return false;
		}
		else if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v)
		{
			return false;
		}
		else if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v)
		{
			return false;
		}
		return true;
	}

	return localRuntime.Native.Show.values = {
		toString: toString
	};
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			_: {},
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a,b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a,b,c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a,b,c,d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a,b,c,d,e]);
	}



	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.String = {};
Elm.Native.String.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr,str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd;
		return (hd = str[0])
			? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
			: Maybe.Nothing;
	}
	function append(a,b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f,str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred,str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f,b,str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f,b,str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}

	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}

	function slice(start, end, str)
	{
		return str.slice(start,end);
	}
	function left(n, str)
	{
		return n < 1 ? "" : str.slice(0,n);
	}
	function right(n, str)
	{
		return n < 1 ? "" : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0,-n);
	}

	function pad(n,chr,str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
	}
	function padRight(n,chr,str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n,chr,str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.trimLeft();
	}
	function trimRight(str)
	{
		return str.trimRight();
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Task = {};
Elm.Native.Task.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			setTimeout(function() {
				if (workQueue.length > 0)
				{
					runTask(workQueue[0], onComplete);
				}
			}, 0);
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== "Nothing")
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);;
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas['href'];
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length == 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  "&#38;")
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  "&#39;")
			.replace(/</g,  "&#60;")
			.replace(/>/g,  "&#62;");
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length == 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] == ' ')
		{
			arr[0] = "&nbsp;"
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] == ' ' && arr[i-1] == ' ')
			{
				arr[i-1] = arr[i-1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] == ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length-1] === " ")
		{
			return arr.slice(0,-1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(localRuntime)
{

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input,state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value,initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2( NS.map, fst, NS.timestamp(ticker) );
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new window.Date(t); },
		read: read
	};

};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1,0,0,0,1,0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t,m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
					  m21*c + m22*s, -m21*s + m22*c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity:identity,
		matrix:F6(matrix),
		rotation:rotation,
		multiply:F2(multiply)
		/*
		transform:F7(transform),
		rotate:F2(rotate),
		move:F2(move),
		scale:F2(scale),
		scaleX:F2(scaleX),
		scaleY:F2(scaleY),
		reflectX:reflectX,
		reflectY:reflectY
		*/
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}

	function eq(l,r)
	{
		var stack = [{'x': l, 'y': r}]
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === "object")
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];

	function compare(x,y)
	{
		return {
			ctor: ord[cmp(x,y)+1]
		};
	}

	function cmp(x,y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === "::" || x.ctor === "[]")
		{
			while (true)
			{
				if (x.ctor === "[]" && y.ctor === "[]")
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0,6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	var Tuple0 = {
		ctor: "_Tuple0"
	};

	function Tuple2(x,y)
	{
		return {
			ctor: "_Tuple2",
			_0: x,
			_1: y
		};
	}

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}

	var count = 0;
	function guid(_)
	{
		return count++
	}

	function copy(oldRecord)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = key === '_'
				? copy(oldRecord._)
				: oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}

	function remove(key, oldRecord)
	{
		var record = copy(oldRecord);
		if (key in record._)
		{
			record[key] = record._[key][0];
			record._[key] = record._[key].slice(1);
			if (record._[key].length === 0)
			{
				delete record._[key];
			}
		}
		else
		{
			delete record[key];
		}
		return record;
	}

	function replace(keyValuePairs, oldRecord)
	{
		var record = copy(oldRecord);
		for (var i = keyValuePairs.length; i--; )
		{
			var pair = keyValuePairs[i];
			record[pair[0]] = pair[1];
		}
		return record;
	}

	function insert(key, value, oldRecord)
	{
		var newRecord = copy(oldRecord);
		if (key in newRecord)
		{
			var values = newRecord._[key];
			var copiedValues = values ? values.slice(0) : [];
			newRecord._[key] = [newRecord[key]].concat(copiedValues);
		}
		newRecord[key] = value;
		return newRecord;
	}

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor:'[]' };

	function Cons(hd,tl)
	{
		return {
			ctor: "::",
			_0: hd,
			_1: tl
		};
	}

	function append(xs,ys)
	{
		// append Strings
		if (typeof xs === "string")
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0,5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}



		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}

	//// RUNTIME ERRORS ////

	function indent(lines)
	{
		return '\n' + lines.join('\n');
	}

	function badCase(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in case-expression.',
			'Make sure your patterns cover every case!'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}

	function badIf(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in multi-way-if expression.',
			'It is best to use \'otherwise\' as the last branch of multi-way-if.'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}


	function badPort(expected, received)
	{
		var msg = indent([
			'Expecting ' + expected + ' but was given ',
			JSON.stringify(received)
		]);
		throw new Error('Runtime error when sending values through a port.' + msg);
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		copy: copy,
		remove: remove,
		replace: replace,
		insert: insert,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		append: F2(append),

		badCase: badCase,
		badIf: badIf,
		badPort: badPort
	};
};

(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports = createHash

function createHash(elem) {
    var attributes = elem.attributes
    var hash = {}

    if (attributes === null || attributes === undefined) {
        return hash
    }

    for (var i = 0; i < attributes.length; i++) {
        var attr = attributes[i]

        if (attr.name.substr(0,5) !== "data-") {
            continue
        }

        hash[attr.name.substr(5)] = attr.value
    }

    return hash
}

},{}],2:[function(require,module,exports){
var createStore = require("weakmap-shim/create-store")
var Individual = require("individual")

var createHash = require("./create-hash.js")

var hashStore = Individual("__DATA_SET_WEAKMAP@3", createStore())

module.exports = DataSet

function DataSet(elem) {
    var store = hashStore(elem)

    if (!store.hash) {
        store.hash = createHash(elem)
    }

    return store.hash
}

},{"./create-hash.js":1,"individual":3,"weakmap-shim/create-store":4}],3:[function(require,module,exports){
(function (global){
var root = typeof window !== 'undefined' ?
    window : typeof global !== 'undefined' ?
    global : {};

module.exports = Individual

function Individual(key, value) {
    if (root[key]) {
        return root[key]
    }

    Object.defineProperty(root, key, {
        value: value
        , configurable: true
    })

    return value
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],4:[function(require,module,exports){
var hiddenStore = require('./hidden-store.js');

module.exports = createStore;

function createStore() {
    var key = {};

    return function (obj) {
        if (typeof obj !== 'object' || obj === null) {
            throw new Error('Weakmap-shim: Key must be object')
        }

        var store = obj.valueOf(key);
        return store && store.identity === key ?
            store : hiddenStore(obj, key);
    };
}

},{"./hidden-store.js":5}],5:[function(require,module,exports){
module.exports = hiddenStore;

function hiddenStore(obj, key) {
    var store = { identity: key };
    var valueOf = obj.valueOf;

    Object.defineProperty(obj, "valueOf", {
        value: function (value) {
            return value !== key ?
                valueOf.apply(this, arguments) : store;
        },
        writable: true
    });

    return store;
}

},{}],6:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = addEvent

function addEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        ds[type] = handler
    } else if (Array.isArray(events)) {
        if (events.indexOf(handler) === -1) {
            events.push(handler)
        }
    } else if (events !== handler) {
        ds[type] = [events, handler]
    }
}

},{"data-set":2}],7:[function(require,module,exports){
var globalDocument = require("global/document")
var DataSet = require("data-set")
var createStore = require("weakmap-shim/create-store")

var addEvent = require("./add-event.js")
var removeEvent = require("./remove-event.js")
var ProxyEvent = require("./proxy-event.js")

var HANDLER_STORE = createStore()

module.exports = DOMDelegator

function DOMDelegator(document) {
    document = document || globalDocument

    this.target = document.documentElement
    this.events = {}
    this.rawEventListeners = {}
    this.globalListeners = {}
}

DOMDelegator.prototype.addEventListener = addEvent
DOMDelegator.prototype.removeEventListener = removeEvent

DOMDelegator.prototype.allocateHandle =
    function allocateHandle(func) {
        var handle = new Handle()

        HANDLER_STORE(handle).func = func;

        return handle
    }

DOMDelegator.prototype.transformHandle =
    function transformHandle(handle, lambda) {
        var func = HANDLER_STORE(handle).func

        return this.allocateHandle(function (ev) {
            var result = lambda(ev)
            if (result) {
                func(result)
            }
        })
    }

DOMDelegator.prototype.addGlobalEventListener =
    function addGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];
        if (listeners.indexOf(fn) === -1) {
            listeners.push(fn)
        }

        this.globalListeners[eventName] = listeners;
    }

DOMDelegator.prototype.removeGlobalEventListener =
    function removeGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];

        var index = listeners.indexOf(fn)
        if (index !== -1) {
            listeners.splice(index, 1)
        }
    }

DOMDelegator.prototype.listenTo = function listenTo(eventName) {
    if (this.events[eventName]) {
        return
    }

    this.events[eventName] = true

    var listener = this.rawEventListeners[eventName]
    if (!listener) {
        listener = this.rawEventListeners[eventName] =
            createHandler(eventName, this)
    }

    this.target.addEventListener(eventName, listener, true)
}

DOMDelegator.prototype.unlistenTo = function unlistenTo(eventName) {
    if (!this.events[eventName]) {
        return
    }

    this.events[eventName] = false
    var listener = this.rawEventListeners[eventName]

    if (!listener) {
        throw new Error("dom-delegator#unlistenTo: cannot " +
            "unlisten to " + eventName)
    }

    this.target.removeEventListener(eventName, listener, true)
}

function createHandler(eventName, delegator) {
    var globalListeners = delegator.globalListeners;
    var delegatorTarget = delegator.target;

    return handler

    function handler(ev) {
        var globalHandlers = globalListeners[eventName] || []

        if (globalHandlers.length > 0) {
            var globalEvent = new ProxyEvent(ev);
            globalEvent.currentTarget = delegatorTarget;
            callListeners(globalHandlers, globalEvent)
        }

        findAndInvokeListeners(ev.target, ev, eventName)
    }
}

function findAndInvokeListeners(elem, ev, eventName) {
    var listener = getListener(elem, eventName)

    if (listener && listener.handlers.length > 0) {
        var listenerEvent = new ProxyEvent(ev);
        listenerEvent.currentTarget = listener.currentTarget
        callListeners(listener.handlers, listenerEvent)

        if (listenerEvent._bubbles) {
            var nextTarget = listener.currentTarget.parentNode
            findAndInvokeListeners(nextTarget, ev, eventName)
        }
    }
}

function getListener(target, type) {
    // terminate recursion if parent is `null`
    if (target === null) {
        return null
    }

    var ds = DataSet(target)
    // fetch list of handler fns for this event
    var handler = ds[type]
    var allHandler = ds.event

    if (!handler && !allHandler) {
        return getListener(target.parentNode, type)
    }

    var handlers = [].concat(handler || [], allHandler || [])
    return new Listener(target, handlers)
}

function callListeners(handlers, ev) {
    handlers.forEach(function (handler) {
        if (typeof handler === "function") {
            handler(ev)
        } else if (typeof handler.handleEvent === "function") {
            handler.handleEvent(ev)
        } else if (handler.type === "dom-delegator-handle") {
            HANDLER_STORE(handler).func(ev)
        } else {
            throw new Error("dom-delegator: unknown handler " +
                "found: " + JSON.stringify(handlers));
        }
    })
}

function Listener(target, handlers) {
    this.currentTarget = target
    this.handlers = handlers
}

function Handle() {
    this.type = "dom-delegator-handle"
}

},{"./add-event.js":6,"./proxy-event.js":15,"./remove-event.js":16,"data-set":2,"global/document":10,"weakmap-shim/create-store":13}],8:[function(require,module,exports){
var Individual = require("individual")
var cuid = require("cuid")
var globalDocument = require("global/document")

var DOMDelegator = require("./dom-delegator.js")

var delegatorCache = Individual("__DOM_DELEGATOR_CACHE@9", {
    delegators: {}
})
var commonEvents = [
    "blur", "change", "click",  "contextmenu", "dblclick",
    "error","focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "load", "mousedown", "mouseup",
    "resize", "scroll", "select", "submit", "touchcancel",
    "touchend", "touchstart", "unload"
]

/*  Delegator is a thin wrapper around a singleton `DOMDelegator`
        instance.

    Only one DOMDelegator should exist because we do not want
        duplicate event listeners bound to the DOM.

    `Delegator` will also `listenTo()` all events unless 
        every caller opts out of it
*/
module.exports = Delegator

function Delegator(opts) {
    opts = opts || {}
    var document = opts.document || globalDocument

    var cacheKey = document["__DOM_DELEGATOR_CACHE_TOKEN@9"]

    if (!cacheKey) {
        cacheKey =
            document["__DOM_DELEGATOR_CACHE_TOKEN@9"] = cuid()
    }

    var delegator = delegatorCache.delegators[cacheKey]

    if (!delegator) {
        delegator = delegatorCache.delegators[cacheKey] =
            new DOMDelegator(document)
    }

    if (opts.defaultEvents !== false) {
        for (var i = 0; i < commonEvents.length; i++) {
            delegator.listenTo(commonEvents[i])
        }
    }

    return delegator
}



},{"./dom-delegator.js":7,"cuid":9,"global/document":10,"individual":11}],9:[function(require,module,exports){
/**
 * cuid.js
 * Collision-resistant UID generator for browsers and node.
 * Sequential for fast db lookups and recency sorting.
 * Safe for element IDs and server-side lookups.
 *
 * Extracted from CLCTR
 * 
 * Copyright (c) Eric Elliott 2012
 * MIT License
 */

/*global window, navigator, document, require, process, module */
(function (app) {
  'use strict';
  var namespace = 'cuid',
    c = 0,
    blockSize = 4,
    base = 36,
    discreteValues = Math.pow(base, blockSize),

    pad = function pad(num, size) {
      var s = "000000000" + num;
      return s.substr(s.length-size);
    },

    randomBlock = function randomBlock() {
      return pad((Math.random() *
            discreteValues << 0)
            .toString(base), blockSize);
    },

    safeCounter = function () {
      c = (c < discreteValues) ? c : 0;
      c++; // this is not subliminal
      return c - 1;
    },

    api = function cuid() {
      // Starting with a lowercase letter makes
      // it HTML element ID friendly.
      var letter = 'c', // hard-coded allows for sequential access

        // timestamp
        // warning: this exposes the exact date and time
        // that the uid was created.
        timestamp = (new Date().getTime()).toString(base),

        // Prevent same-machine collisions.
        counter,

        // A few chars to generate distinct ids for different
        // clients (so different computers are far less
        // likely to generate the same id)
        fingerprint = api.fingerprint(),

        // Grab some more chars from Math.random()
        random = randomBlock() + randomBlock();

        counter = pad(safeCounter().toString(base), blockSize);

      return  (letter + timestamp + counter + fingerprint + random);
    };

  api.slug = function slug() {
    var date = new Date().getTime().toString(36),
      counter,
      print = api.fingerprint().slice(0,1) +
        api.fingerprint().slice(-1),
      random = randomBlock().slice(-2);

      counter = safeCounter().toString(36).slice(-4);

    return date.slice(-2) + 
      counter + print + random;
  };

  api.globalCount = function globalCount() {
    // We want to cache the results of this
    var cache = (function calc() {
        var i,
          count = 0;

        for (i in window) {
          count++;
        }

        return count;
      }());

    api.globalCount = function () { return cache; };
    return cache;
  };

  api.fingerprint = function browserPrint() {
    return pad((navigator.mimeTypes.length +
      navigator.userAgent.length).toString(36) +
      api.globalCount().toString(36), 4);
  };

  // don't change anything from here down.
  if (app.register) {
    app.register(namespace, api);
  } else if (typeof module !== 'undefined') {
    module.exports = api;
  } else {
    app[namespace] = api;
  }

}(this.applitude || this));

},{}],10:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":40}],11:[function(require,module,exports){
module.exports=require(3)
},{}],12:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],13:[function(require,module,exports){
module.exports=require(4)
},{"./hidden-store.js":14}],14:[function(require,module,exports){
module.exports=require(5)
},{}],15:[function(require,module,exports){
var inherits = require("inherits")

var ALL_PROPS = [
    "altKey", "bubbles", "cancelable", "ctrlKey",
    "eventPhase", "metaKey", "relatedTarget", "shiftKey",
    "target", "timeStamp", "type", "view", "which"
]
var KEY_PROPS = ["char", "charCode", "key", "keyCode"]
var MOUSE_PROPS = [
    "button", "buttons", "clientX", "clientY", "layerX",
    "layerY", "offsetX", "offsetY", "pageX", "pageY",
    "screenX", "screenY", "toElement"
]

var rkeyEvent = /^key|input/
var rmouseEvent = /^(?:mouse|pointer|contextmenu)|click/

module.exports = ProxyEvent

function ProxyEvent(ev) {
    if (!(this instanceof ProxyEvent)) {
        return new ProxyEvent(ev)
    }

    if (rkeyEvent.test(ev.type)) {
        return new KeyEvent(ev)
    } else if (rmouseEvent.test(ev.type)) {
        return new MouseEvent(ev)
    }

    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    this._rawEvent = ev
    this._bubbles = false;
}

ProxyEvent.prototype.preventDefault = function () {
    this._rawEvent.preventDefault()
}

ProxyEvent.prototype.startPropagation = function () {
    this._bubbles = true;
}

function MouseEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < MOUSE_PROPS.length; j++) {
        var mousePropKey = MOUSE_PROPS[j]
        this[mousePropKey] = ev[mousePropKey]
    }

    this._rawEvent = ev
}

inherits(MouseEvent, ProxyEvent)

function KeyEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < KEY_PROPS.length; j++) {
        var keyPropKey = KEY_PROPS[j]
        this[keyPropKey] = ev[keyPropKey]
    }

    this._rawEvent = ev
}

inherits(KeyEvent, ProxyEvent)

},{"inherits":12}],16:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = removeEvent

function removeEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        return
    } else if (Array.isArray(events)) {
        var index = events.indexOf(handler)
        if (index !== -1) {
            events.splice(index, 1)
        }
    } else if (events === handler) {
        ds[type] = null
    }
}

},{"data-set":2}],17:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("vtree/is-vhook")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, props, previous, propName);
        } else if (isHook(propValue)) {
            propValue.hook(node,
                propName,
                previous ? previous[propName] : undefined)
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else if (propValue !== undefined) {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, props, previous, propName) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"is-object":21,"vtree/is-vhook":29}],18:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("vtree/is-vnode")
var isVText = require("vtree/is-vtext")
var isWidget = require("vtree/is-widget")
var handleThunk = require("vtree/handle-thunk")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"./apply-properties":17,"global/document":20,"vtree/handle-thunk":27,"vtree/is-vnode":30,"vtree/is-vtext":31,"vtree/is-widget":32}],19:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],20:[function(require,module,exports){
module.exports=require(10)
},{"min-document":40}],21:[function(require,module,exports){
module.exports = isObject

function isObject(x) {
    return typeof x === "object" && x !== null
}

},{}],22:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],23:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("vtree/is-widget")
var VPatch = require("vtree/vpatch")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    if (updateWidget(leftVNode, widget)) {
        return widget.update(leftVNode, domNode) || domNode
    }

    var parentNode = domNode.parentNode
    var newWidget = render(widget, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newWidget, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newWidget
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, bIndex) {
    var children = []
    var childNodes = domNode.childNodes
    var len = childNodes.length
    var i
    var reverseIndex = bIndex.reverse

    for (i = 0; i < len; i++) {
        children.push(domNode.childNodes[i])
    }

    var insertOffset = 0
    var move
    var node
    var insertNode
    for (i = 0; i < len; i++) {
        move = bIndex[i]
        if (move !== undefined && move !== i) {
            // the element currently at this index will be moved later so increase the insert offset
            if (reverseIndex[i] > i) {
                insertOffset++
            }

            node = children[move]
            insertNode = childNodes[i + insertOffset] || null
            if (node !== insertNode) {
                domNode.insertBefore(node, insertNode)
            }

            // the moved element came from the front of the array so reduce the insert offset
            if (move < i) {
                insertOffset--
            }
        }

        // element at this index is scheduled to be removed so increase insert offset
        if (i in bIndex.removes) {
            insertOffset++
        }
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        console.log(oldRoot)
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"./apply-properties":17,"./create-element":18,"./update-widget":25,"vtree/is-widget":32,"vtree/vpatch":37}],24:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":19,"./patch-op":23,"global/document":20,"x-is-array":22}],25:[function(require,module,exports){
var isWidget = require("vtree/is-widget")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"vtree/is-widget":32}],26:[function(require,module,exports){
var isArray = require("x-is-array")
var isObject = require("is-object")

var VPatch = require("./vpatch")
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var handleThunk = require("./handle-thunk")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        if (isThunk(a) || isThunk(b)) {
            thunks(a, b, patch, index)
        } else {
            hooks(b, patch, index)
        }
        return
    }

    var apply = patch[index]

    if (b == null) {
        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
        destroyWidgets(a, patch, index)
    } else if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties, b.hooks)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                destroyWidgets(a, patch, index)
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            destroyWidgets(a, patch, index)
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            destroyWidgets(a, patch, index)
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))

        if (!isWidget(a)) {
            destroyWidgets(a, patch, index)
        }
    }

    if (apply) {
        patch[index] = apply
    }
}

function diffProps(a, b, hooks) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (hooks && aKey in hooks) {
            diff = diff || {}
            diff[aKey] = bValue
        } else {
            if (isObject(aValue) && isObject(bValue)) {
                if (getPrototype(bValue) !== getPrototype(aValue)) {
                    diff = diff || {}
                    diff[aKey] = bValue
                } else {
                    var objectDiff = diffProps(aValue, bValue)
                    if (objectDiff) {
                        diff = diff || {}
                        diff[aKey] = objectDiff
                    }
                }
            } else if (aValue !== bValue) {
                diff = diff || {}
                diff[aKey] = bValue
            }
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var bChildren = reorder(aChildren, b.children)

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else if (!rightNode) {
            if (leftNode) {
                // Excess nodes in a need to be removed
                patch[index] = new VPatch(VPatch.REMOVE, leftNode, null)
                destroyWidgets(leftNode, patch, index)
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (bChildren.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(VPatch.ORDER, a, bChildren.moves))
    }

    return apply
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = new VPatch(VPatch.REMOVE, vNode, null)
        }
    } else if (isVNode(vNode) && vNode.hasWidgets) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b);
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true;
        }
    }

    return false;
}

// Execute hooks when two nodes are identical
function hooks(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = new VPatch(VPatch.PROPS, vNode.hooks, vNode.hooks)
        }

        if (vNode.descendantHooks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                hooks(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    }
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {

    var bKeys = keyIndex(bChildren)

    if (!bKeys) {
        return bChildren
    }

    var aKeys = keyIndex(aChildren)

    if (!aKeys) {
        return bChildren
    }

    var bMatch = {}, aMatch = {}

    for (var key in bKeys) {
        bMatch[bKeys[key]] = aKeys[key]
    }

    for (var key in aKeys) {
        aMatch[aKeys[key]] = bKeys[key]
    }

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen
    var shuffle = []
    var freeIndex = 0
    var i = 0
    var moveIndex = 0
    var moves = {}
    var removes = moves.removes = {}
    var reverse = moves.reverse = {}
    var hasMoves = false

    while (freeIndex < len) {
        var move = aMatch[i]
        if (move !== undefined) {
            shuffle[i] = bChildren[move]
            if (move !== moveIndex) {
                moves[move] = moveIndex
                reverse[moveIndex] = move
                hasMoves = true
            }
            moveIndex++
        } else if (i in aMatch) {
            shuffle[i] = undefined
            removes[i] = moveIndex++
            hasMoves = true
        } else {
            while (bMatch[freeIndex] !== undefined) {
                freeIndex++
            }

            if (freeIndex < len) {
                var freeChild = bChildren[freeIndex]
                if (freeChild) {
                    shuffle[i] = freeChild
                    if (freeIndex !== moveIndex) {
                        hasMoves = true
                        moves[freeIndex] = moveIndex
                        reverse[moveIndex] = freeIndex
                    }
                    moveIndex++
                }
                freeIndex++
            }
        }
        i++
    }

    if (hasMoves) {
        shuffle.moves = moves
    }

    return shuffle
}

function keyIndex(children) {
    var i, keys

    for (i = 0; i < children.length; i++) {
        var child = children[i]

        if (child.key !== undefined) {
            keys = keys || {}
            keys[child.key] = i
        }
    }

    return keys
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"./handle-thunk":27,"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32,"./vpatch":37,"is-object":33,"x-is-array":34}],27:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32}],28:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],29:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook && typeof hook.hook === "function" &&
        !hook.hasOwnProperty("hook")
}

},{}],30:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":35}],31:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":35}],32:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],33:[function(require,module,exports){
module.exports=require(21)
},{}],34:[function(require,module,exports){
module.exports=require(22)
},{}],35:[function(require,module,exports){
module.exports = "1"

},{}],36:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property)) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-vhook":29,"./is-vnode":30,"./is-widget":32,"./version":35}],37:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":35}],38:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":35}],39:[function(require,module,exports){

var VNode = require('vtree/vnode');
var VText = require('vtree/vtext');
var diff = require('vtree/diff');
var patch = require('vdom/patch');
var createElement = require('vdom/create-element');
var DataSet = require("data-set");
var Delegator = require("dom-delegator");
var isHook = require("vtree/is-vhook");

Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	// This manages event listeners. Somehow...
	// Save a reference for use in on(...)
	var delegator = Delegator();

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}

	function node(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			name === 'input'
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}

	function on(name, decoder, createMessage)
	{
		// Ensure we're listening for this type of event
		delegator.listenTo(name);
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property(name, DataSetHook(eventHandler));
	}

	function DataSetHook(value)
	{
		if (!(this instanceof DataSetHook))
		{
			return new DataSetHook(value);
		}

		this.value = value;
	}

	DataSetHook.prototype.hook = function (node, propertyName) {
		var ds = DataSet(node);
		ds[propertyName] = this.value;
	};


	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};

	function text(string)
	{
		return new VText(string);
	}

	function fromElement(element)
	{
		return {
			type: "Widget",

			element: element,

			init: function () {
				return Element.render(element);
			},

			update: function (previous, node) {
				return Element.update(node, previous.element, element);
			}
		};
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}

	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}

	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		this.fn = fn;
		this.args = args;
		this.vnode = null;
		this.key = undefined;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.update = updateThunk;
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function updateThunk(previous, domNode)
	{
		if (!shouldUpdate(this, previous))
		{
			this.vnode = previous.vnode;
			return;
		}

		if (!this.vnode)
		{
			this.vnode = this.thunk();
		}

		var patches = diff(previous.vnode, this.vnode);
		patch(domNode, patches);
	}

	function renderThunk()
	{
		return this.thunk();
	}

	return Elm.Native.VirtualDom.values = {
		node: F3(node),
		text: text,
		on: F3(on),

		property: F2(property),
		attribute: F2(attribute),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"data-set":2,"dom-delegator":8,"vdom/create-element":18,"vdom/patch":24,"vtree/diff":26,"vtree/is-vhook":29,"vtree/vnode":36,"vtree/vtext":38}],40:[function(require,module,exports){

},{}]},{},[39]);

Elm.Native = Elm.Native || {};
Elm.Native.Window = {};
Elm.Native.Window.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Window = localRuntime.Native.Window || {};
	if (localRuntime.Native.Window.values)
	{
		return localRuntime.Native.Window.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;


	function getWidth()
	{
		return localRuntime.node.clientWidth;
	}


	function getHeight()
	{
		if (localRuntime.isFullscreen())
		{
			return window.innerHeight;
		}
		return localRuntime.node.clientHeight;
	}


	var dimensions = NS.input('Window.dimensions', Tuple2(getWidth(), getHeight()));


	function resizeIfNeeded()
	{
		// Do not trigger event if the dimensions have not changed.
		// This should be most of the time.
		var w = getWidth();
		var h = getHeight();
		if (dimensions.value._0 === w && dimensions.value._1 === h)
		{
			return;
		}

		setTimeout(function () {
			// Check again to see if the dimensions have changed.
			// It is conceivable that the dimensions have changed
			// again while some other event was being processed.
			var w = getWidth();
			var h = getHeight();
			if (dimensions.value._0 === w && dimensions.value._1 === h)
			{
				return;
			}
			localRuntime.notify(dimensions.id, Tuple2(w,h));
		}, 0);
	}


	localRuntime.addListener([dimensions.id], window, 'resize', resizeIfNeeded);


	return localRuntime.Native.Window.values = {
		dimensions: dimensions,
		resizeIfNeeded: resizeIfNeeded
	};
};

Elm.Piece = Elm.Piece || {};
Elm.Piece.make = function (_elm) {
   "use strict";
   _elm.Piece = _elm.Piece || {};
   if (_elm.Piece.values)
   return _elm.Piece.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Piece",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Piece$Internal = Elm.Piece.Internal.make(_elm),
   $Queue = Elm.Queue.make(_elm),
   $Queue$Internal = Elm.Queue.Internal.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm);
   var filterMap = F3(function (f,
   x,
   s) {
      return $Signal.map(function (v) {
         return function () {
            switch (v.ctor)
            {case "Just": return v._0;}
            _U.badCase($moduleName,
            "on line 216, column 24 to 45");
         }();
      })(A2($Signal.filter,
      function (v) {
         return function () {
            switch (v.ctor)
            {case "Just": return true;}
            return false;
         }();
      },
      $Maybe.Just(x))(A2($Signal.map,
      f,
      s)));
   });
   var popTil = function (f) {
      return function () {
         var go = function (q) {
            return function () {
               var _v4 = $Queue.pop(q);
               switch (_v4.ctor)
               {case "Just":
                  switch (_v4._0.ctor)
                    {case "_Tuple2":
                       return f(_v4._0._0) ? $Maybe.Just({ctor: "_Tuple2"
                                                         ,_0: _v4._0._0
                                                         ,_1: _v4._0._1}) : go(_v4._0._1);}
                    break;
                  case "Nothing":
                  return $Maybe.Nothing;}
               _U.badCase($moduleName,
               "between lines 207 and 210");
            }();
         };
         return go;
      }();
   };
   var TheOtherThing = function (a) {
      return {ctor: "TheOtherThing"
             ,_0: a};
   };
   var TheOneThing = function (a) {
      return {ctor: "TheOneThing"
             ,_0: a};
   };
   var duration = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "Piece":
            switch (_v8._0.ctor)
              {case "ForATime":
                 return _v8._0._0;}
              break;}
         _U.badCase($moduleName,
         "on line 186, column 39 to 40");
      }();
   };
   var layer = F2(function (tSig,
   pSig) {
      return function () {
         var update = F2(function (u,
         _v13) {
            return function () {
               switch (_v13.ctor)
               {case "_Tuple3":
                  return function () {
                       switch (u.ctor)
                       {case "TheOneThing":
                          return {ctor: "_Tuple3"
                                 ,_0: A2($List.filter,
                                 function (_v21) {
                                    return function () {
                                       switch (_v21.ctor)
                                       {case "_Tuple2":
                                          return _U.cmp(u._0 - _v21._0,
                                            duration(_v21._1)) < 0;}
                                       _U.badCase($moduleName,
                                       "on line 195, column 57 to 77");
                                    }();
                                 },
                                 _v13._0)
                                 ,_1: u._0
                                 ,_2: true};
                          case "TheOtherThing":
                          return {ctor: "_Tuple3"
                                 ,_0: A2($List._op["::"],
                                 u._0,
                                 _v13._0)
                                 ,_1: _v13._1
                                 ,_2: false};}
                       _U.badCase($moduleName,
                       "between lines 194 and 197");
                    }();}
               _U.badCase($moduleName,
               "between lines 194 and 197");
            }();
         });
         return A2(filterMap,
         function (_v25) {
            return function () {
               switch (_v25.ctor)
               {case "_Tuple3":
                  return _v25._2 ? $Maybe.Just(A2($List.map,
                    function (_v30) {
                       return function () {
                          switch (_v30.ctor)
                          {case "_Tuple2":
                             switch (_v30._1.ctor)
                               {case "Piece":
                                  return _v30._1._1(_v25._1 - _v30._0);}
                               break;}
                          _U.badCase($moduleName,
                          "on line 202, column 61 to 70");
                       }();
                    },
                    _v25._0)) : $Maybe.Nothing;}
               _U.badCase($moduleName,
               "on line 202, column 8 to 89");
            }();
         },
         _L.fromArray([]))(A3($Signal.foldp,
         update,
         {ctor: "_Tuple3"
         ,_0: _L.fromArray([])
         ,_1: 0
         ,_2: false},
         A2($Signal.merge,
         A2($Signal.map,
         TheOtherThing,
         $Time.timestamp(pSig)),
         A2($Signal.map,
         TheOneThing,
         tSig))));
      }();
   });
   var CPiece = function (a) {
      return {ctor: "CPiece"
             ,_0: a};
   };
   var CTime = function (a) {
      return {ctor: "CTime",_0: a};
   };
   var run = F2(function (ts,s) {
      return A3($Signal.map2,
      F2(function (_v36,t) {
         return function () {
            switch (_v36.ctor)
            {case "_Tuple2":
               switch (_v36._1.ctor)
                 {case "Piece":
                    return _v36._1._1(t - _v36._0);}
                 break;}
            _U.badCase($moduleName,
            "on line 164, column 40 to 49");
         }();
      }),
      A3($Signal.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      A2($Signal.sampleOn,s,ts),
      s),
      ts);
   });
   var mkF = F4(function (d1,
   f1,
   f2,
   t) {
      return _U.cmp(t,
      d1) < 1 ? f1(t) : f2(t - d1);
   });
   var followedBy = F2(function (_v42,
   _v43) {
      return function () {
         switch (_v43.ctor)
         {case "Piece":
            return function () {
                 switch (_v42.ctor)
                 {case "Piece":
                    return function () {
                         var _v50 = {ctor: "_Tuple2"
                                    ,_0: _v42._0
                                    ,_1: _v43._0};
                         switch (_v50.ctor)
                         {case "_Tuple2":
                            switch (_v50._0.ctor)
                              {case "ForATime":
                                 switch (_v50._1.ctor)
                                   {case "ForATime":
                                      return A2($Piece$Internal.Piece,
                                        $Piece$Internal.ForATime(_v50._0._0 + _v50._1._0),
                                        A3(mkF,
                                        _v50._0._0,
                                        _v42._1,
                                        _v43._1));
                                      case "Forever":
                                      return A2($Piece$Internal.Piece,
                                        $Piece$Internal.Forever,
                                        A3(mkF,
                                        _v50._0._0,
                                        _v42._1,
                                        _v43._1));}
                                   break;
                                 case "Forever":
                                 return $Debug.crash("The impossible happened: Piece.followedBy");}
                              break;}
                         _U.badCase($moduleName,
                         "between lines 141 and 153");
                      }();}
                 _U.badCase($moduleName,
                 "between lines 141 and 153");
              }();}
         _U.badCase($moduleName,
         "between lines 141 and 153");
      }();
   });
   var chainTo = F2(function (_v55,
   g) {
      return function () {
         switch (_v55.ctor)
         {case "Piece":
            return function () {
                 switch (_v55._0.ctor)
                 {case "ForATime":
                    return A2(followedBy,
                      A2($Piece$Internal.Piece,
                      _v55._0,
                      _v55._1),
                      g(_v55._1(_v55._0._0)));}
                 return $Debug.crash("The impossible happened: Piece.chainTo");
              }();}
         _U.badCase($moduleName,
         "between lines 156 and 161");
      }();
   });
   var map = F2(function (g,_v61) {
      return function () {
         switch (_v61.ctor)
         {case "Piece":
            return A2($Piece$Internal.Piece,
              _v61._0,
              function ($) {
                 return g(_v61._1($));
              });}
         _U.badCase($moduleName,
         "on line 120, column 25 to 44");
      }();
   });
   var dilate = F2(function (s,
   _v65) {
      return function () {
         switch (_v65.ctor)
         {case "Piece":
            return function () {
                 var dur$ = _U.eq(s,
                 0) ? $Piece$Internal.Forever : function () {
                    switch (_v65._0.ctor)
                    {case "ForATime":
                       return $Piece$Internal.ForATime(_v65._0._0 / s);}
                    return _v65._0;
                 }();
                 return A2($Piece$Internal.Piece,
                 dur$,
                 function (t) {
                    return _v65._1(s * t);
                 });
              }();}
         _U.badCase($moduleName,
         "between lines 106 and 110");
      }();
   });
   var finalValue = function (_v71) {
      return function () {
         switch (_v71.ctor)
         {case "Piece":
            switch (_v71._0.ctor)
              {case "ForATime":
                 return _v71._1(_v71._0._0);}
              break;}
         _U.badCase($moduleName,
         "on line 96, column 41 to 44");
      }();
   };
   var $for = F2(function (dur,f) {
      return A2($Piece$Internal.Piece,
      $Piece$Internal.ForATime(dur),
      f);
   });
   var stayFor = F2(function (dur,
   x) {
      return A2($for,
      dur,
      function (_v76) {
         return function () {
            return x;
         }();
      });
   });
   var endToEnd = function (_v78) {
      return function () {
         switch (_v78.ctor)
         {case "Piece":
            return function () {
                 var update = F2(function (u,
                 _v82) {
                    return function () {
                       switch (_v82.ctor)
                       {case "_Tuple4":
                          return function () {
                               var _ = _v82._2;
                               var d = function () {
                                  switch (_.ctor)
                                  {case "Piece":
                                     switch (_._0.ctor)
                                       {case "ForATime":
                                          return _._0._0;}
                                       break;}
                                  _U.badCase($moduleName,
                                  "on line 173, column 42 to 43");
                               }();
                               var f = function () {
                                  switch (_.ctor)
                                  {case "Piece":
                                     switch (_._0.ctor)
                                       {case "ForATime": return _._1;}
                                       break;}
                                  _U.badCase($moduleName,
                                  "on line 173, column 42 to 43");
                               }();
                               return function () {
                                  switch (u.ctor)
                                  {case "CPiece":
                                     return {ctor: "_Tuple4"
                                            ,_0: _v82._0
                                            ,_1: _v82._1
                                            ,_2: _v82._2
                                            ,_3: A2($Queue.push,
                                            u._0,
                                            _v82._3)};
                                     case "CTime":
                                     return _U.cmp(u._0 - _v82._1,
                                       d) < 0 ? {ctor: "_Tuple4"
                                                ,_0: f(u._0)
                                                ,_1: _v82._1
                                                ,_2: _v82._2
                                                ,_3: _v82._3} : function () {
                                          var _v99 = $Queue.pop(_v82._3);
                                          switch (_v99.ctor)
                                          {case "Just":
                                             switch (_v99._0.ctor)
                                               {case "_Tuple2":
                                                  return function () {
                                                       var _ = _v99._0._0;
                                                       var g = function () {
                                                          switch (_.ctor)
                                                          {case "Piece":
                                                             return _._1;}
                                                          _U.badCase($moduleName,
                                                          "on line 177, column 55 to 57");
                                                       }();
                                                       return {ctor: "_Tuple4"
                                                              ,_0: g(u._0)
                                                              ,_1: u._0
                                                              ,_2: _v99._0._0
                                                              ,_3: _v99._0._1};
                                                    }();}
                                               break;
                                             case "Nothing":
                                             return {ctor: "_Tuple4"
                                                    ,_0: _v78._1(u._0)
                                                    ,_1: _v82._1
                                                    ,_2: _v82._2
                                                    ,_3: $Queue.empty};}
                                          _U.badCase($moduleName,
                                          "between lines 175 and 178");
                                       }();}
                                  _U.badCase($moduleName,
                                  "between lines 174 and 179");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 173 and 179");
                    }();
                 });
                 return F2(function (ts,ss) {
                    return $Signal.map(function (_v106) {
                       return function () {
                          switch (_v106.ctor)
                          {case "_Tuple4":
                             return _v106._0;}
                          _U.badCase($moduleName,
                          "on line 183, column 34 to 35");
                       }();
                    })(A3($Signal.foldp,
                    update,
                    {ctor: "_Tuple4"
                    ,_0: _v78._1(0)
                    ,_1: 0
                    ,_2: A2($for,0,_v78._1)
                    ,_3: $Queue.empty},
                    A2($Signal.merge,
                    A2($Signal.map,CPiece,ss),
                    A2($Signal.map,CTime,ts))));
                 });
              }();}
         _U.badCase($moduleName,
         "between lines 172 and 183");
      }();
   };
   var forever = function (f) {
      return A2($Piece$Internal.Piece,
      $Piece$Internal.Forever,
      f);
   };
   var stayForever = function (x) {
      return forever(function (_v112) {
         return function () {
            return x;
         }();
      });
   };
   var sustain = function (st) {
      return A2(followedBy,
      st,
      stayForever(finalValue(st)));
   };
   var ForATimeDummy = {ctor: "ForATimeDummy"};
   var ForeverDummy = {ctor: "ForeverDummy"};
   var modFloat = F2(function (x,
   m) {
      return x - m * $Basics.toFloat($Basics.floor(x / m));
   });
   var cycle = function (_v114) {
      return function () {
         switch (_v114.ctor)
         {case "Piece":
            return function () {
                 switch (_v114._0.ctor)
                 {case "ForATime":
                    return A2($Piece$Internal.Piece,
                      $Piece$Internal.Forever,
                      function (t) {
                         return _v114._1(A2(modFloat,
                         t,
                         _v114._0._0));
                      });}
                 return $Debug.crash("The impossible happened: Piece.cycle");
              }();}
         _U.badCase($moduleName,
         "between lines 115 and 117");
      }();
   };
   _elm.Piece.values = {_op: _op
                       ,forever: forever
                       ,$for: $for
                       ,stayFor: stayFor
                       ,stayForever: stayForever
                       ,sustain: sustain
                       ,dilate: dilate
                       ,cycle: cycle
                       ,map: map
                       ,followedBy: followedBy
                       ,chainTo: chainTo
                       ,run: run
                       ,finalValue: finalValue
                       ,layer: layer};
   return _elm.Piece.values;
};
Elm.Piece = Elm.Piece || {};
Elm.Piece.Infix = Elm.Piece.Infix || {};
Elm.Piece.Infix.make = function (_elm) {
   "use strict";
   _elm.Piece = _elm.Piece || {};
   _elm.Piece.Infix = _elm.Piece.Infix || {};
   if (_elm.Piece.Infix.values)
   return _elm.Piece.Infix.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Piece.Infix",
   $Piece = Elm.Piece.make(_elm);
   _op["+>"] = $Piece.chainTo;
   _op[">+>"] = F3(function (f,
   g,
   x) {
      return A2(_op["+>"],f(x),g);
   });
   _op["<>"] = $Piece.followedBy;
   _elm.Piece.Infix.values = {_op: _op};
   return _elm.Piece.Infix.values;
};
Elm.Piece = Elm.Piece || {};
Elm.Piece.Internal = Elm.Piece.Internal || {};
Elm.Piece.Internal.make = function (_elm) {
   "use strict";
   _elm.Piece = _elm.Piece || {};
   _elm.Piece.Internal = _elm.Piece.Internal || {};
   if (_elm.Piece.Internal.values)
   return _elm.Piece.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Piece.Internal",
   $Time = Elm.Time.make(_elm);
   var Piece = F2(function (a,b) {
      return {ctor: "Piece"
             ,_0: a
             ,_1: b};
   });
   var Forever = {ctor: "Forever"};
   var ForATime = function (a) {
      return {ctor: "ForATime"
             ,_0: a};
   };
   _elm.Piece.Internal.values = {_op: _op
                                ,ForATime: ForATime
                                ,Forever: Forever
                                ,Piece: Piece};
   return _elm.Piece.Internal.values;
};
Elm.PieceUtils = Elm.PieceUtils || {};
Elm.PieceUtils.make = function (_elm) {
   "use strict";
   _elm.PieceUtils = _elm.PieceUtils || {};
   if (_elm.PieceUtils.values)
   return _elm.PieceUtils.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "PieceUtils",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Piece$Internal = Elm.Piece.Internal.make(_elm);
   var durMin = F2(function (d1,
   d2) {
      return function () {
         var _v0 = {ctor: "_Tuple2"
                   ,_0: d1
                   ,_1: d2};
         switch (_v0.ctor)
         {case "_Tuple2":
            switch (_v0._0.ctor)
              {case "Forever": return d2;}
              switch (_v0._1.ctor)
              {case "Forever": return d1;}
              switch (_v0._0.ctor)
              {case "ForATime":
                 switch (_v0._1.ctor)
                   {case "ForATime":
                      return $Piece$Internal.ForATime(A2($Basics.min,
                        _v0._0._0,
                        _v0._1._0));}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 6 and 9");
      }();
   });
   var sequence = function (ss) {
      return function () {
         var _ = ss;
         var d0 = function () {
            switch (_.ctor)
            {case "::": switch (_._0.ctor)
                 {case "Piece": return _._0._0;}
                 break;}
            _U.badCase($moduleName,
            "on line 15, column 29 to 31");
         }();
         var ss$ = function () {
            switch (_.ctor)
            {case "::": switch (_._0.ctor)
                 {case "Piece": return _._1;}
                 break;}
            _U.badCase($moduleName,
            "on line 15, column 29 to 31");
         }();
         return A2($Piece$Internal.Piece,
         A3($List.foldl,
         F2(function (_v15,r) {
            return function () {
               switch (_v15.ctor)
               {case "Piece": return A2(durMin,
                    _v15._0,
                    r);}
               _U.badCase($moduleName,
               "on line 16, column 40 to 50");
            }();
         }),
         d0,
         ss$),
         function (t) {
            return A2($List.map,
            function (_v19) {
               return function () {
                  switch (_v19.ctor)
                  {case "Piece":
                     return _v19._1(t);}
                  _U.badCase($moduleName,
                  "on line 17, column 38 to 41");
               }();
            },
            ss);
         });
      }();
   };
   var mapM = function (f) {
      return function ($) {
         return sequence($List.map(f)($));
      };
   };
   _elm.PieceUtils.values = {_op: _op
                            ,durMin: durMin
                            ,sequence: sequence
                            ,mapM: mapM};
   return _elm.PieceUtils.values;
};
Elm.Queue = Elm.Queue || {};
Elm.Queue.make = function (_elm) {
   "use strict";
   _elm.Queue = _elm.Queue || {};
   if (_elm.Queue.values)
   return _elm.Queue.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Queue",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Queue$Internal = Elm.Queue.Internal.make(_elm);
   var toList = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "Queue":
            return A2($Basics._op["++"],
              _v0._0,
              $List.reverse(_v0._1));}
         _U.badCase($moduleName,
         "on line 55, column 22 to 41");
      }();
   };
   var map = F2(function (g,_v4) {
      return function () {
         switch (_v4.ctor)
         {case "Queue":
            return A2($Queue$Internal.Queue,
              A2($List.map,g,_v4._0),
              A2($List.map,g,_v4._1));}
         _U.badCase($moduleName,
         "on line 52, column 21 to 55");
      }();
   });
   var length = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "Queue":
            return $List.length(_v8._0) + $List.length(_v8._1);}
         _U.badCase($moduleName,
         "on line 49, column 22 to 51");
      }();
   };
   var isEmpty = function (q) {
      return function () {
         switch (q.ctor)
         {case "Queue":
            switch (q._0.ctor)
              {case "[]": switch (q._1.ctor)
                   {case "[]": return true;}
                   break;}
              break;}
         return false;
      }();
   };
   var pop = function (_v15) {
      return function () {
         switch (_v15.ctor)
         {case "Queue":
            return function () {
                 switch (_v15._0.ctor)
                 {case "::":
                    return $Maybe.Just({ctor: "_Tuple2"
                                       ,_0: _v15._0._0
                                       ,_1: A2($Queue$Internal.Queue,
                                       _v15._0._1,
                                       _v15._1)});
                    case "[]": return function () {
                         switch (_v15._1.ctor)
                         {case "[]":
                            return $Maybe.Nothing;}
                         return function () {
                            var _raw = $List.reverse(_v15._1),
                            $ = _raw.ctor === "::" ? _raw : _U.badCase($moduleName,
                            "on line 40, column 27 to 41"),
                            x = $._0,
                            f$ = $._1;
                            return $Maybe.Just({ctor: "_Tuple2"
                                               ,_0: x
                                               ,_1: A2($Queue$Internal.Queue,
                                               f$,
                                               _L.fromArray([]))});
                         }();
                      }();}
                 _U.badCase($moduleName,
                 "between lines 37 and 41");
              }();}
         _U.badCase($moduleName,
         "between lines 37 and 41");
      }();
   };
   var push = F2(function (x,
   _v23) {
      return function () {
         switch (_v23.ctor)
         {case "Queue":
            return A2($Queue$Internal.Queue,
              _v23._0,
              A2($List._op["::"],x,_v23._1));}
         _U.badCase($moduleName,
         "on line 34, column 23 to 36");
      }();
   });
   var empty = A2($Queue$Internal.Queue,
   _L.fromArray([]),
   _L.fromArray([]));
   _elm.Queue.values = {_op: _op
                       ,empty: empty
                       ,push: push
                       ,pop: pop
                       ,isEmpty: isEmpty
                       ,length: length
                       ,map: map
                       ,toList: toList};
   return _elm.Queue.values;
};
Elm.Queue = Elm.Queue || {};
Elm.Queue.Internal = Elm.Queue.Internal || {};
Elm.Queue.Internal.make = function (_elm) {
   "use strict";
   _elm.Queue = _elm.Queue || {};
   _elm.Queue.Internal = _elm.Queue.Internal || {};
   if (_elm.Queue.Internal.values)
   return _elm.Queue.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Queue.Internal";
   var Queue = F2(function (a,b) {
      return {ctor: "Queue"
             ,_0: a
             ,_1: b};
   });
   _elm.Queue.Internal.values = {_op: _op
                                ,Queue: Queue};
   return _elm.Queue.Internal.values;
};
Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values)
   return _elm.Random.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Random",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm);
   var magicNum8 = 2147483562;
   var range = function (_v0) {
      return function () {
         return {ctor: "_Tuple2"
                ,_0: 0
                ,_1: magicNum8};
      }();
   };
   var magicNum7 = 2137383399;
   var magicNum6 = 2147483563;
   var magicNum5 = 3791;
   var magicNum4 = 40692;
   var magicNum3 = 52774;
   var magicNum2 = 12211;
   var magicNum1 = 53668;
   var magicNum0 = 40014;
   var generate = F2(function (_v2,
   seed) {
      return function () {
         switch (_v2.ctor)
         {case "Generator":
            return _v2._0(seed);}
         _U.badCase($moduleName,
         "on line 246, column 5 to 19");
      }();
   });
   var Seed = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,next: b
             ,range: d
             ,split: c
             ,state: a};
   });
   var State = F2(function (a,b) {
      return {ctor: "State"
             ,_0: a
             ,_1: b};
   });
   var initState = function (s$) {
      return function () {
         var s = A2($Basics.max,
         s$,
         0 - s$);
         var q = s / (magicNum6 - 1) | 0;
         var s2 = A2($Basics._op["%"],
         q,
         magicNum7 - 1);
         var s1 = A2($Basics._op["%"],
         s,
         magicNum6 - 1);
         return A2(State,s1 + 1,s2 + 1);
      }();
   };
   var next = function (_v5) {
      return function () {
         switch (_v5.ctor)
         {case "State":
            return function () {
                 var k$ = _v5._1 / magicNum3 | 0;
                 var s2$ = magicNum4 * (_v5._1 - k$ * magicNum3) - k$ * magicNum5;
                 var s2$$ = _U.cmp(s2$,
                 0) < 0 ? s2$ + magicNum7 : s2$;
                 var k = _v5._0 / magicNum1 | 0;
                 var s1$ = magicNum0 * (_v5._0 - k * magicNum1) - k * magicNum2;
                 var s1$$ = _U.cmp(s1$,
                 0) < 0 ? s1$ + magicNum6 : s1$;
                 var z = s1$$ - s2$$;
                 var z$ = _U.cmp(z,
                 1) < 0 ? z + magicNum8 : z;
                 return {ctor: "_Tuple2"
                        ,_0: z$
                        ,_1: A2(State,s1$$,s2$$)};
              }();}
         _U.badCase($moduleName,
         "between lines 290 and 299");
      }();
   };
   var split = function (_v9) {
      return function () {
         switch (_v9.ctor)
         {case "State":
            return function () {
                 var _raw = $Basics.snd(next(_v9)),
                 $ = _raw.ctor === "State" ? _raw : _U.badCase($moduleName,
                 "on line 306, column 25 to 38"),
                 t1 = $._0,
                 t2 = $._1;
                 var new_s2 = _U.eq(_v9._1,
                 1) ? magicNum7 - 1 : _v9._1 - 1;
                 var new_s1 = _U.eq(_v9._0,
                 magicNum6 - 1) ? 1 : _v9._0 + 1;
                 return {ctor: "_Tuple2"
                        ,_0: A2(State,new_s1,t2)
                        ,_1: A2(State,t1,new_s2)};
              }();}
         _U.badCase($moduleName,
         "between lines 304 and 308");
      }();
   };
   var initialSeed = function (n) {
      return A4(Seed,
      initState(n),
      next,
      split,
      range);
   };
   var Generator = function (a) {
      return {ctor: "Generator"
             ,_0: a};
   };
   var customGenerator = function (generate) {
      return Generator(generate);
   };
   var listHelp = F4(function (list,
   n,
   generate,
   seed) {
      return _U.cmp(n,
      1) < 0 ? {ctor: "_Tuple2"
               ,_0: $List.reverse(list)
               ,_1: seed} : function () {
         var $ = generate(seed),
         value = $._0,
         seed$ = $._1;
         return A4(listHelp,
         A2($List._op["::"],value,list),
         n - 1,
         generate,
         seed$);
      }();
   });
   var list = F2(function (n,
   _v13) {
      return function () {
         switch (_v13.ctor)
         {case "Generator":
            return Generator(function (seed) {
                 return A4(listHelp,
                 _L.fromArray([]),
                 n,
                 _v13._0,
                 seed);
              });}
         _U.badCase($moduleName,
         "between lines 182 and 183");
      }();
   });
   var pair = F2(function (_v16,
   _v17) {
      return function () {
         switch (_v17.ctor)
         {case "Generator":
            return function () {
                 switch (_v16.ctor)
                 {case "Generator":
                    return Generator(function (seed) {
                         return function () {
                            var $ = _v16._0(seed),
                            left = $._0,
                            seed$ = $._1;
                            var $ = _v17._0(seed$),
                            right = $._0,
                            seed$$ = $._1;
                            return {ctor: "_Tuple2"
                                   ,_0: {ctor: "_Tuple2"
                                        ,_0: left
                                        ,_1: right}
                                   ,_1: seed$$};
                         }();
                      });}
                 _U.badCase($moduleName,
                 "between lines 159 and 163");
              }();}
         _U.badCase($moduleName,
         "between lines 159 and 163");
      }();
   });
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var iLogBase = F2(function (b,
   i) {
      return _U.cmp(i,
      b) < 0 ? 1 : 1 + A2(iLogBase,
      b,
      i / b | 0);
   });
   var $int = F2(function (a,b) {
      return Generator(function (seed) {
         return function () {
            var base = 2147483561;
            var f = F3(function (n,
            acc,
            state) {
               return function () {
                  switch (n)
                  {case 0: return {ctor: "_Tuple2"
                                  ,_0: acc
                                  ,_1: state};}
                  return function () {
                     var $ = seed.next(state),
                     x = $._0,
                     state$ = $._1;
                     return A3(f,
                     n - 1,
                     x + acc * base,
                     state$);
                  }();
               }();
            });
            var $ = _U.cmp(a,
            b) < 0 ? {ctor: "_Tuple2"
                     ,_0: a
                     ,_1: b} : {ctor: "_Tuple2"
                               ,_0: b
                               ,_1: a},
            lo = $._0,
            hi = $._1;
            var k = hi - lo + 1;
            var n = A2(iLogBase,base,k);
            var $ = A3(f,n,1,seed.state),
            v = $._0,
            state$ = $._1;
            return {ctor: "_Tuple2"
                   ,_0: lo + A2($Basics._op["%"],
                   v,
                   k)
                   ,_1: _U.replace([["state"
                                    ,state$]],
                   seed)};
         }();
      });
   });
   var $float = F2(function (a,b) {
      return Generator(function (seed) {
         return function () {
            var $ = A2(generate,
            A2($int,minInt,maxInt),
            seed),
            number = $._0,
            seed$ = $._1;
            var negativeOneToOne = $Basics.toFloat(number) / $Basics.toFloat(maxInt - minInt);
            var $ = _U.cmp(a,
            b) < 0 ? {ctor: "_Tuple2"
                     ,_0: a
                     ,_1: b} : {ctor: "_Tuple2"
                               ,_0: b
                               ,_1: a},
            lo = $._0,
            hi = $._1;
            var scaled = (lo + hi) / 2 + (hi - lo) * negativeOneToOne;
            return {ctor: "_Tuple2"
                   ,_0: scaled
                   ,_1: seed$};
         }();
      });
   });
   _elm.Random.values = {_op: _op
                        ,$int: $int
                        ,$float: $float
                        ,list: list
                        ,pair: pair
                        ,minInt: minInt
                        ,maxInt: maxInt
                        ,generate: generate
                        ,initialSeed: initialSeed
                        ,customGenerator: customGenerator
                        ,Seed: Seed};
   return _elm.Random.values;
};
Elm.Ratio = Elm.Ratio || {};
Elm.Ratio.make = function (_elm) {
   "use strict";
   _elm.Ratio = _elm.Ratio || {};
   if (_elm.Ratio.values)
   return _elm.Ratio.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Ratio",
   $Basics = Elm.Basics.make(_elm);
   var toFloat = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "Ratio":
            return $Basics.toFloat(_v0._0) / $Basics.toFloat(_v0._1);}
         _U.badCase($moduleName,
         "on line 77, column 23 to 58");
      }();
   };
   var split = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "Ratio":
            return {ctor: "_Tuple2"
                   ,_0: _v4._0
                   ,_1: _v4._1};}
         _U.badCase($moduleName,
         "on line 74, column 22 to 26");
      }();
   };
   var denominator = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "Ratio": return _v8._1;}
         _U.badCase($moduleName,
         "on line 71, column 27 to 28");
      }();
   };
   var numerator = function (_v12) {
      return function () {
         switch (_v12.ctor)
         {case "Ratio": return _v12._0;}
         _U.badCase($moduleName,
         "on line 68, column 25 to 26");
      }();
   };
   var gcd = F2(function (a,b) {
      return _U.eq(b,
      0) ? a : A2(gcd,
      b,
      A2($Basics._op["%"],a,b));
   });
   var Ratio = F2(function (a,b) {
      return {ctor: "Ratio"
             ,_0: a
             ,_1: b};
   });
   var normalize = function (_v16) {
      return function () {
         switch (_v16.ctor)
         {case "Ratio":
            return function () {
                 var k = A2(gcd,
                 _v16._0,
                 _v16._1) * (_U.cmp(_v16._1,
                 0) < 0 ? -1 : 1);
                 return A2(Ratio,
                 _v16._0 / k | 0,
                 _v16._1 / k | 0);
              }();}
         _U.badCase($moduleName,
         "on line 45, column 3 to 73");
      }();
   };
   var add = F2(function (_v20,
   _v21) {
      return function () {
         switch (_v21.ctor)
         {case "Ratio":
            return function () {
                 switch (_v20.ctor)
                 {case "Ratio":
                    return normalize(A2(Ratio,
                      _v20._0 * _v21._1 + _v20._1 * _v21._0,
                      _v20._1 * _v21._1));}
                 _U.badCase($moduleName,
                 "on line 49, column 3 to 42");
              }();}
         _U.badCase($moduleName,
         "on line 49, column 3 to 42");
      }();
   });
   var multiply = F2(function (_v28,
   _v29) {
      return function () {
         switch (_v29.ctor)
         {case "Ratio":
            return function () {
                 switch (_v28.ctor)
                 {case "Ratio":
                    return normalize(A2(Ratio,
                      _v28._0 * _v29._0,
                      _v28._1 * _v29._1));}
                 _U.badCase($moduleName,
                 "on line 53, column 3 to 34");
              }();}
         _U.badCase($moduleName,
         "on line 53, column 3 to 34");
      }();
   });
   var divide = F2(function (r,
   _v36) {
      return function () {
         switch (_v36.ctor)
         {case "Ratio":
            return A2(multiply,
              r,
              A2(Ratio,_v36._1,_v36._0));}
         _U.badCase($moduleName,
         "on line 56, column 24 to 45");
      }();
   });
   var negate = multiply(A2(Ratio,
   -1,
   1));
   var over = F2(function (x,y) {
      return normalize(A2(Ratio,
      x,
      y));
   });
   var fromInt = function (x) {
      return A2(over,x,1);
   };
   _elm.Ratio.values = {_op: _op
                       ,gcd: gcd
                       ,add: add
                       ,multiply: multiply
                       ,divide: divide
                       ,negate: negate
                       ,over: over
                       ,denominator: denominator
                       ,numerator: numerator
                       ,split: split
                       ,toFloat: toFloat
                       ,fromInt: fromInt};
   return _elm.Ratio.values;
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 164 and 177");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 126 and 145");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 41 and 52");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 55 and 58");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 63 and 67");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 72 and 77");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 82 and 123");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 148 and 161");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 180 and 182");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe
                        ,formatError: formatError
                        ,Ok: Ok
                        ,Err: Err};
   return _elm.Result.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var send = F2(function (_v0,
   value) {
      return function () {
         switch (_v0.ctor)
         {case "Address":
            return A2($Task.onError,
              _v0._0(value),
              function (_v3) {
                 return function () {
                    return $Task.succeed({ctor: "_Tuple0"});
                 }();
              });}
         _U.badCase($moduleName,
         "between lines 370 and 371");
      }();
   });
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var message = F2(function (_v5,
   value) {
      return function () {
         switch (_v5.ctor)
         {case "Address":
            return Message(_v5._0(value));}
         _U.badCase($moduleName,
         "on line 352, column 5 to 24");
      }();
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address"
             ,_0: a};
   };
   var forwardTo = F2(function (_v8,
   f) {
      return function () {
         switch (_v8.ctor)
         {case "Address":
            return Address(function (x) {
                 return _v8._0(f(x));
              });}
         _U.badCase($moduleName,
         "on line 339, column 5 to 29");
      }();
   });
   var Mailbox = F2(function (a,
   b) {
      return {_: {}
             ,address: a
             ,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,
   base,
   signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,
   right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      return function () {
         var _v11 = $List.reverse(signalList);
         switch (_v11.ctor)
         {case "::":
            return A3($List.foldl,
              merge,
              _v11._0,
              _v11._1);
            case "[]":
            return $Debug.crash("mergeMany was given an empty list!");}
         _U.badCase($moduleName,
         "between lines 177 and 197");
      }();
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   _op["~"] = F2(function (funcs,
   args) {
      return A3(map2,
      F2(function (f,v) {
         return f(v);
      }),
      funcs,
      args);
   });
   var map = $Native$Signal.map;
   _op["<~"] = map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,constant: constant
                        ,dropRepeats: dropRepeats
                        ,filter: filter
                        ,filterMap: filterMap
                        ,sampleOn: sampleOn
                        ,foldp: foldp
                        ,mailbox: mailbox
                        ,send: send
                        ,message: message
                        ,forwardTo: forwardTo
                        ,Mailbox: Mailbox};
   return _elm.Signal.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Extra = Elm.Signal.Extra || {};
Elm.Signal.Extra.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Extra = _elm.Signal.Extra || {};
   if (_elm.Signal.Extra.values)
   return _elm.Signal.Extra.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal.Extra",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var combine = A2($List.foldr,
   $Signal.map2(F2(function (x,y) {
      return A2($List._op["::"],
      x,
      y);
   })),
   $Signal.constant(_L.fromArray([])));
   var mapMany = F2(function (f,
   l) {
      return A2($Signal._op["<~"],
      f,
      combine(l));
   });
   var applyMany = F2(function (fs,
   l) {
      return A2($Signal._op["~"],
      fs,
      combine(l));
   });
   var filter = function (initial) {
      return A2($Signal.filterMap,
      $Basics.identity,
      initial);
   };
   var keepIf = $Signal.filter;
   var runBuffer$ = F3(function (l,
   n,
   input) {
      return function () {
         var f = F2(function (inp,
         prev) {
            return function () {
               var l = $List.length(prev);
               return _U.cmp(l,
               n) < 0 ? A2($Basics._op["++"],
               prev,
               _L.fromArray([inp])) : A2($Basics._op["++"],
               A2($List.drop,l - n + 1,prev),
               _L.fromArray([inp]));
            }();
         });
         return A3($Signal.foldp,
         f,
         l,
         input);
      }();
   });
   var runBuffer = runBuffer$(_L.fromArray([]));
   var foldps = F3(function (f,
   bs,
   aS) {
      return A2($Signal._op["<~"],
      $Basics.fst,
      A3($Signal.foldp,
      F2(function (a,_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2": return A2(f,
                 a,
                 _v0._1);}
            _U.badCase($moduleName,
            "on line 120, column 29 to 34");
         }();
      }),
      bs,
      aS));
   });
   var delayRound = F2(function (b,
   bS) {
      return A3(foldps,
      F2(function ($new,old) {
         return {ctor: "_Tuple2"
                ,_0: old
                ,_1: $new};
      }),
      {ctor: "_Tuple2",_0: b,_1: b},
      bS);
   });
   var filterFold = F2(function (f,
   initial) {
      return function () {
         var f$ = F2(function (a,s) {
            return function () {
               var res = A2(f,a,s);
               return {ctor: "_Tuple2"
                      ,_0: res
                      ,_1: A2($Maybe.withDefault,
                      s,
                      res)};
            }();
         });
         return function ($) {
            return filter(initial)(A2(foldps,
            f$,
            {ctor: "_Tuple2"
            ,_0: $Maybe.Just(initial)
            ,_1: initial})($));
         };
      }();
   });
   var initSignal = function (s) {
      return A2($Signal.sampleOn,
      $Signal.constant({ctor: "_Tuple0"}),
      s);
   };
   var switchHelper = F4(function (filter,
   b,
   l,
   r) {
      return function () {
         var fromJust = function (_v4) {
            return function () {
               switch (_v4.ctor)
               {case "Just": return _v4._0;}
               _U.badCase($moduleName,
               "on line 211, column 25 to 26");
            }();
         };
         var lAndR = A2($Signal.merge,
         A3(filter,
         b,
         $Maybe.Nothing,
         A2($Signal._op["<~"],
         $Maybe.Just,
         l)),
         A3(filter,
         A2($Signal._op["<~"],
         $Basics.not,
         b),
         $Maybe.Nothing,
         A2($Signal._op["<~"],
         $Maybe.Just,
         r)));
         var base = A2($Signal._op["~"],
         A2($Signal._op["~"],
         A2($Signal._op["<~"],
         F3(function (bi,li,ri) {
            return $Maybe.Just(bi ? li : ri);
         }),
         initSignal(b)),
         initSignal(l)),
         initSignal(r));
         return A2($Signal._op["<~"],
         fromJust,
         A2($Signal.merge,base,lAndR));
      }();
   });
   var unzip4 = function (pairS) {
      return {ctor: "_Tuple4"
             ,_0: A2($Signal._op["<~"],
             function (_v7) {
                return function () {
                   switch (_v7.ctor)
                   {case "_Tuple4": return _v7._0;}
                   _U.badCase($moduleName,
                   "on line 80, column 19 to 20");
                }();
             },
             pairS)
             ,_1: A2($Signal._op["<~"],
             function (_v13) {
                return function () {
                   switch (_v13.ctor)
                   {case "_Tuple4":
                      return _v13._1;}
                   _U.badCase($moduleName,
                   "on line 80, column 47 to 48");
                }();
             },
             pairS)
             ,_2: A2($Signal._op["<~"],
             function (_v19) {
                return function () {
                   switch (_v19.ctor)
                   {case "_Tuple4":
                      return _v19._2;}
                   _U.badCase($moduleName,
                   "on line 80, column 75 to 76");
                }();
             },
             pairS)
             ,_3: A2($Signal._op["<~"],
             function (_v25) {
                return function () {
                   switch (_v25.ctor)
                   {case "_Tuple4":
                      return _v25._3;}
                   _U.badCase($moduleName,
                   "on line 80, column 103 to 104");
                }();
             },
             pairS)};
   };
   var unzip3 = function (pairS) {
      return {ctor: "_Tuple3"
             ,_0: A2($Signal._op["<~"],
             function (_v31) {
                return function () {
                   switch (_v31.ctor)
                   {case "_Tuple3":
                      return _v31._0;}
                   _U.badCase($moduleName,
                   "on line 74, column 17 to 18");
                }();
             },
             pairS)
             ,_1: A2($Signal._op["<~"],
             function (_v36) {
                return function () {
                   switch (_v36.ctor)
                   {case "_Tuple3":
                      return _v36._1;}
                   _U.badCase($moduleName,
                   "on line 74, column 43 to 44");
                }();
             },
             pairS)
             ,_2: A2($Signal._op["<~"],
             function (_v41) {
                return function () {
                   switch (_v41.ctor)
                   {case "_Tuple3":
                      return _v41._2;}
                   _U.badCase($moduleName,
                   "on line 74, column 69 to 70");
                }();
             },
             pairS)};
   };
   var unzip = function (pairS) {
      return {ctor: "_Tuple2"
             ,_0: A2($Signal._op["<~"],
             $Basics.fst,
             pairS)
             ,_1: A2($Signal._op["<~"],
             $Basics.snd,
             pairS)};
   };
   var zip4 = $Signal.map4(F4(function (v0,
   v1,
   v2,
   v3) {
      return {ctor: "_Tuple4"
             ,_0: v0
             ,_1: v1
             ,_2: v2
             ,_3: v3};
   }));
   var zip3 = $Signal.map3(F3(function (v0,
   v1,
   v2) {
      return {ctor: "_Tuple3"
             ,_0: v0
             ,_1: v1
             ,_2: v2};
   }));
   var zip = $Signal.map2(F2(function (v0,
   v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }));
   var keepWhen = F3(function (boolSig,
   a,
   aSig) {
      return $Signal.map($Basics.snd)(A2(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2"
      ,_0: true
      ,_1: a})($Signal.sampleOn(aSig)(A2(zip,
      boolSig,
      aSig))));
   });
   var switchWhen = F3(function (b,
   l,
   r) {
      return A4(switchHelper,
      keepWhen,
      b,
      l,
      r);
   });
   var sampleWhen = F3(function (bs,
   def,
   sig) {
      return $Signal.map($Basics.snd)(A2(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2"
      ,_0: true
      ,_1: def})(A2(zip,bs,sig)));
   });
   var switchSample = F3(function (b,
   l,
   r) {
      return A4(switchHelper,
      sampleWhen,
      b,
      l,
      r);
   });
   var keepThen = F3(function (choice,
   base,
   signal) {
      return A2(switchSample,
      choice,
      signal)($Signal.constant(base));
   });
   _op["~>"] = $Basics.flip($Signal.map);
   var foldp$ = F3(function (fun,
   initFun,
   input) {
      return function () {
         var fromJust = function (_v46) {
            return function () {
               switch (_v46.ctor)
               {case "Just": return _v46._0;}
               _U.badCase($moduleName,
               "on line 107, column 25 to 26");
            }();
         };
         var fun$ = F2(function (_v49,
         mb) {
            return function () {
               switch (_v49.ctor)
               {case "_Tuple2":
                  return $Maybe.Just(fun(_v49._0)(A2($Maybe.withDefault,
                    _v49._1,
                    mb)));}
               _U.badCase($moduleName,
               "between lines 104 and 105");
            }();
         });
         var initial = A2(_op["~>"],
         initSignal(input),
         initFun);
         var rest = A3($Signal.foldp,
         fun$,
         $Maybe.Nothing,
         A2(zip,input,initial));
         return A2($Signal._op["<~"],
         fromJust,
         A2($Signal.merge,
         A2($Signal._op["<~"],
         $Maybe.Just,
         initial),
         rest));
      }();
   });
   var foldps$ = F3(function (f,
   iF,
   aS) {
      return A2($Signal._op["<~"],
      $Basics.fst,
      A3(foldp$,
      F2(function (a,_v53) {
         return function () {
            switch (_v53.ctor)
            {case "_Tuple2": return A2(f,
                 a,
                 _v53._1);}
            _U.badCase($moduleName,
            "on line 126, column 46 to 51");
         }();
      }),
      iF,
      aS));
   });
   var foldpWith = F4(function (unpack,
   step,
   init,
   input) {
      return function () {
         var step$ = F2(function (a,
         _v57) {
            return function () {
               switch (_v57.ctor)
               {case "_Tuple2":
                  return unpack(A2(step,
                    a,
                    _v57._1));}
               _U.badCase($moduleName,
               "on line 140, column 7 to 25");
            }();
         });
         return A2(_op["~>"],
         A3($Signal.foldp,
         step$,
         init,
         input),
         $Basics.fst);
      }();
   });
   var keepWhenI = F2(function (fs,
   s) {
      return function () {
         var fromJust = function (_v61) {
            return function () {
               switch (_v61.ctor)
               {case "Just": return _v61._0;}
               _U.badCase($moduleName,
               "on line 257, column 25 to 26");
            }();
         };
         return A2(_op["~>"],
         A3(keepWhen,
         A2($Signal.merge,
         $Signal.constant(true),
         fs),
         $Maybe.Nothing,
         A2($Signal._op["<~"],
         $Maybe.Just,
         s)),
         fromJust);
      }();
   });
   var fairMerge = F3(function (resolve,
   left,
   right) {
      return function () {
         var merged = A2($Signal.merge,
         left,
         right);
         var boolRight = A2($Signal._op["<~"],
         $Basics.always(false),
         right);
         var boolLeft = A2($Signal._op["<~"],
         $Basics.always(true),
         left);
         var bothUpdated = A2($Signal._op["~"],
         A2($Signal._op["<~"],
         F2(function (x,y) {
            return !_U.eq(x,y);
         }),
         A2($Signal.merge,
         boolLeft,
         boolRight)),
         A2($Signal.merge,
         boolRight,
         boolLeft));
         var keep = keepWhenI(bothUpdated);
         var resolved = A2($Signal._op["~"],
         A2($Signal._op["<~"],
         resolve,
         keep(left)),
         keep(right));
         return $Signal.merge(resolved)(merged);
      }();
   });
   _elm.Signal.Extra.values = {_op: _op
                              ,zip: zip
                              ,zip3: zip3
                              ,zip4: zip4
                              ,unzip: unzip
                              ,unzip3: unzip3
                              ,unzip4: unzip4
                              ,foldp$: foldp$
                              ,foldps: foldps
                              ,foldps$: foldps$
                              ,runBuffer: runBuffer
                              ,runBuffer$: runBuffer$
                              ,delayRound: delayRound
                              ,keepIf: keepIf
                              ,keepWhen: keepWhen
                              ,sampleWhen: sampleWhen
                              ,switchWhen: switchWhen
                              ,keepWhenI: keepWhenI
                              ,switchSample: switchSample
                              ,keepThen: keepThen
                              ,filter: filter
                              ,filterFold: filterFold
                              ,fairMerge: fairMerge
                              ,combine: combine
                              ,mapMany: mapMany
                              ,applyMany: applyMany};
   return _elm.Signal.Extra.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,length: length
                        ,reverse: reverse
                        ,repeat: repeat
                        ,cons: cons
                        ,uncons: uncons
                        ,fromChar: fromChar
                        ,append: append
                        ,concat: concat
                        ,split: split
                        ,join: join
                        ,words: words
                        ,lines: lines
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,map: map
                        ,filter: filter
                        ,foldl: foldl
                        ,foldr: foldr
                        ,any: any
                        ,all: all};
   return _elm.String.values;
};
Elm.Style = Elm.Style || {};
Elm.Style.make = function (_elm) {
   "use strict";
   _elm.Style = _elm.Style || {};
   if (_elm.Style.values)
   return _elm.Style.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Style",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Config = Elm.Config.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $String = Elm.String.make(_elm),
   $Util = Elm.Util.make(_elm);
   var darken = function (c) {
      return function () {
         var $ = $Color.toHsl(c),
         hue = $.hue,
         saturation = $.saturation,
         lightness = $.lightness,
         alpha = $.alpha;
         return A4($Color.hsla,
         hue,
         saturation,
         0.5 * lightness,
         1);
      }();
   };
   var lighten = function (c) {
      return function () {
         var $ = $Color.toHsl(c),
         hue = $.hue,
         saturation = $.saturation,
         lightness = $.lightness,
         alpha = $.alpha;
         return A4($Color.hsla,
         hue,
         saturation,
         2 * lightness,
         1);
      }();
   };
   var customButtonH = 50;
   var customButtonW = 150;
   var levelButtonW = 40;
   var levelButtonH = 40;
   var winTextColor = A3($Color.rgb,
   75,
   91,
   110);
   var buttonBackgroundColor = winTextColor;
   var movesLeftTextColor = winTextColor;
   var defTextStyle = function (h) {
      return {_: {}
             ,bold: false
             ,color: winTextColor
             ,height: $Maybe.Just(h)
             ,italic: false
             ,line: $Maybe.Nothing
             ,typeface: _L.fromArray(["Josefin Sans"
                                     ,"sans-serif"])};
   };
   var defaultFontStr = A2($String.join,
   ",",
   defTextStyle(0).typeface);
   var fadeColor = A3($Color.rgb,
   254,
   204,
   9);
   var rotateArcColor = fadeColor;
   var movesLeftCircleColor = fadeColor;
   var globalStyle = function () {
      var buttonColor = A3($Color.rgb,
      0,
      119,
      219);
      return A2($Html.toElement,
      0,
      0)($Util.styleNode(_L.fromArray([{ctor: "_Tuple2"
                                       ,_0: ".win-difficulty-button"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "pointer-events"
                                                          ,_1: "auto"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".swbutton"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "pointer-events"
                                                          ,_1: "auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "width"
                                                          ,_1: $Util.px(customButtonW)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "height"
                                                          ,_1: $Util.px(customButtonH)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "line-height"
                                                          ,_1: $Util.px(customButtonH)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr(buttonColor)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "text-align"
                                                          ,_1: "center"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "border-radius"
                                                          ,_1: $Util.px(9)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin"
                                                          ,_1: "0 auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "font-size"
                                                          ,_1: $Util.px(20)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.white)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "cursor"
                                                          ,_1: "pointer"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".swbutton:hover"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr(lighten(buttonColor))}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".swbutton:active"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr(darken(buttonColor))}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".top-isom"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "border-top-left-radius"
                                                          ,_1: $Util.px(8)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "border-top-right-radius"
                                                          ,_1: $Util.px(8)}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".bottom-isom"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "border-bottom-left-radius"
                                                          ,_1: $Util.px(8)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "border-bottom-right-radius"
                                                          ,_1: $Util.px(8)}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".isom"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "cursor"
                                                          ,_1: "pointer"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".movebutton"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "display"
                                                          ,_1: "inline-block"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin"
                                                          ,_1: $Util.px(7)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "pointer-events"
                                                          ,_1: "auto"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".movebutton:hover"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "opacity"
                                                          ,_1: $Basics.toString(0.6)}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#explanationtext"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "width"
                                                          ,_1: $Util.px($Config.w - 60)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin-left"
                                                          ,_1: "auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin-right"
                                                          ,_1: "auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "text-align"
                                                          ,_1: "left"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#titletext"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "font-size"
                                                          ,_1: $Util.px(70)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "text-align"
                                                          ,_1: "center"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin"
                                                          ,_1: "0 auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "font-weight"
                                                          ,_1: "800"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "width"
                                                          ,_1: $Util.px(levelButtonH)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "height"
                                                          ,_1: $Util.px(levelButtonW)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "line-height"
                                                          ,_1: $Util.px(levelButtonH)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin"
                                                          ,_1: "0"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "pointer-events"
                                                          ,_1: "auto"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "text-align"
                                                          ,_1: "center"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button.active"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.black)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "cursor"
                                                          ,_1: "pointer"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button.inactive"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.grayscale(0.2))}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "cursor"
                                                          ,_1: "pointer"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button.active:hover"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.grayscale(0.1))}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr($Color.grayscale(0.5))}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button.active:active"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.white)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr($Color.black)}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".level-button.current"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "color"
                                                          ,_1: $Util.colorStr($Color.white)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr($Color.black)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "cursor"
                                                          ,_1: "default"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "body"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "font-family"
                                                          ,_1: defaultFontStr}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#win-screen"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "width"
                                                          ,_1: $Util.px($Config.w)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "height"
                                                          ,_1: $Util.px($Config.h)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-color"
                                                          ,_1: $Util.colorStr(fadeColor)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "text-align"
                                                          ,_1: "center"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "position"
                                                          ,_1: "absolute"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "top"
                                                          ,_1: "0"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#win-screen-inner"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "position"
                                                          ,_1: "relative"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "top"
                                                          ,_1: "30%"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#win-screen h1"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "font-size"
                                                          ,_1: $Util.px(80)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "font-weight"
                                                          ,_1: "bold"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin"
                                                          ,_1: "0"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#win-screen .score"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "font-size"
                                                          ,_1: $Util.px(50)}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#tweet .icon"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "background-image"
                                                          ,_1: "url(\"images/twitter.png\")"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: "#facebook .icon"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "background-image"
                                                          ,_1: "url(\"images/facebook.png\")"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".icon"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "background-repeat"
                                                          ,_1: "no-repeat"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-size"
                                                          ,_1: "auto 100%"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "background-position"
                                                          ,_1: "center center"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "pointer-events"
                                                          ,_1: "auto"}])}
                                      ,{ctor: "_Tuple2"
                                       ,_0: ".iconcon"
                                       ,_1: _L.fromArray([{ctor: "_Tuple2"
                                                          ,_0: "display"
                                                          ,_1: "inline-block"}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin-left"
                                                          ,_1: $Util.px(10)}
                                                         ,{ctor: "_Tuple2"
                                                          ,_0: "margin-right"
                                                          ,_1: $Util.px(10)}])}])));
   }();
   var borderColor = A3($Color.rgb,
   188,
   188,
   188);
   var backgroundColor = A3($Color.rgb,
   223,
   223,
   223);
   _elm.Style.values = {_op: _op
                       ,backgroundColor: backgroundColor
                       ,borderColor: borderColor
                       ,fadeColor: fadeColor
                       ,winTextColor: winTextColor
                       ,buttonBackgroundColor: buttonBackgroundColor
                       ,rotateArcColor: rotateArcColor
                       ,movesLeftTextColor: movesLeftTextColor
                       ,movesLeftCircleColor: movesLeftCircleColor
                       ,defTextStyle: defTextStyle
                       ,defaultFontStr: defaultFontStr
                       ,globalStyle: globalStyle
                       ,levelButtonH: levelButtonH
                       ,levelButtonW: levelButtonW
                       ,customButtonW: customButtonW
                       ,customButtonH: customButtonH
                       ,lighten: lighten
                       ,darken: darken};
   return _elm.Style.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values)
   return _elm.Task.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID"
             ,_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,
   promise) {
      return A2(onError,
      promise,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,
   promiseA) {
      return A2(andThen,
      promiseA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,
   promiseA,
   promiseB) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,
   promiseA,
   promiseB,
   promiseC) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return succeed(A3(func,
               a,
               b,
               c));
            });
         });
      });
   });
   var map4 = F5(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return succeed(A4(func,
                  a,
                  b,
                  c,
                  d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD,
   promiseE) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return A2(andThen,
                  promiseE,
                  function (e) {
                     return succeed(A5(func,
                     a,
                     b,
                     c,
                     d,
                     e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (promiseFunc,
   promiseValue) {
      return A2(andThen,
      promiseFunc,
      function (func) {
         return A2(andThen,
         promiseValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (promises) {
      return function () {
         switch (promises.ctor)
         {case "::": return A3(map2,
              F2(function (x,y) {
                 return A2($List._op["::"],
                 x,
                 y);
              }),
              promises._0,
              sequence(promises._1));
            case "[]":
            return succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 101 and 106");
      }();
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_v3) {
         return function () {
            return succeed($Maybe.Nothing);
         }();
      });
   };
   var fromMaybe = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return succeed(maybe._0);
            case "Nothing":
            return fail($default);}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return fail(result._0);
            case "Ok":
            return succeed(result._0);}
         _U.badCase($moduleName,
         "between lines 151 and 153");
      }();
   };
   var Task = {ctor: "Task"};
   _elm.Task.values = {_op: _op
                      ,succeed: succeed
                      ,fail: fail
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,andMap: andMap
                      ,sequence: sequence
                      ,andThen: andThen
                      ,onError: onError
                      ,mapError: mapError
                      ,toMaybe: toMaybe
                      ,fromMaybe: fromMaybe
                      ,toResult: toResult
                      ,fromResult: fromResult
                      ,spawn: spawn
                      ,sleep: sleep};
   return _elm.Task.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,link: link
                      ,style: style
                      ,defaultStyle: defaultStyle
                      ,typeface: typeface
                      ,monospace: monospace
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,Style: Style
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Signal.delay;
   var since = F2(function (time,
   signal) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,time,signal));
         var start = A2($Signal.map,
         $Basics.always(1),
         signal);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,
      targetFrames,
      $Signal.constant(true));
   };
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,timestamp: timestamp
                      ,delay: delay
                      ,since: since};
   return _elm.Time.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
Elm.Util = Elm.Util || {};
Elm.Util.make = function (_elm) {
   "use strict";
   _elm.Util = _elm.Util || {};
   if (_elm.Util.values)
   return _elm.Util.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Util",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$IsomUtil = Elm.Native.IsomUtil.make(_elm),
   $Ratio = Elm.Ratio.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var foldl1 = function (f) {
      return function () {
         var go = F2(function (acc,
         xs) {
            return function () {
               switch (xs.ctor)
               {case "::": return A2(go,
                    A2(f,xs._0,acc),
                    xs._1);
                  case "[]": return acc;}
               _U.badCase($moduleName,
               "between lines 144 and 147");
            }();
         });
         return function (xs) {
            return function () {
               switch (xs.ctor)
               {case "::": return A2(go,
                    xs._0,
                    xs._1);
                  case "[]":
                  return $Debug.crash("Util.foldl1: Empty list");}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         };
      }();
   };
   var foldr1 = function (f) {
      return function () {
         var go = function (xs) {
            return function () {
               switch (xs.ctor)
               {case "::": switch (xs._1.ctor)
                    {case "[]": return xs._0;}
                    return A2(f,xs._0,go(xs._1));}
               _U.badCase($moduleName,
               "between lines 135 and 138");
            }();
         };
         return function (xs) {
            return function () {
               switch (xs.ctor)
               {case "[]":
                  return $Debug.crash("Util.foldr1: Empty list");}
               return go(xs);
            }();
         };
      }();
   };
   var splitAt = F2(function (k,
   xs) {
      return _U.eq(k,
      0) ? {ctor: "_Tuple2"
           ,_0: _L.fromArray([])
           ,_1: xs} : function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var $ = A2(splitAt,
                 k - 1,
                 xs._1),
                 a = $._0,
                 b = $._1;
                 return {ctor: "_Tuple2"
                        ,_0: A2($List._op["::"],xs._0,a)
                        ,_1: b};
              }();
            case "[]":
            return {ctor: "_Tuple2"
                   ,_0: _L.fromArray([])
                   ,_1: xs};}
         _U.badCase($moduleName,
         "between lines 118 and 120");
      }();
   });
   var groupsOf = F2(function (k,
   xs) {
      return function () {
         var $ = A2(splitAt,k,xs),
         a = $._0,
         b = $._1;
         return function () {
            switch (b.ctor)
            {case "[]":
               return _L.fromArray([a]);}
            return A2($List._op["::"],
            a,
            A2(groupsOf,k,b));
         }();
      }();
   });
   var signalMap6 = F7(function (f,
   s1,
   s2,
   s3,
   s4,
   s5,
   s6) {
      return A2($Signal._op["~"],
      A6($Signal.map5,
      f,
      s1,
      s2,
      s3,
      s4,
      s5),
      s6);
   });
   var signalMap7 = F8(function (f,
   s1,
   s2,
   s3,
   s4,
   s5,
   s6,
   s7) {
      return A2($Signal._op["~"],
      A7(signalMap6,
      f,
      s1,
      s2,
      s3,
      s4,
      s5,
      s6),
      s7);
   });
   var listInit = F2(function (f,
   n) {
      return function () {
         var go = function (i) {
            return _U.eq(i,
            n) ? _L.fromArray([]) : A2($List._op["::"],
            f(i),
            go(i + 1));
         };
         return go(0);
      }();
   });
   var onLastLevel = function (s) {
      return _U.eq($Array.length(s.levels),
      s.currLevelIndex);
   };
   var px = function (n) {
      return A2($Basics._op["++"],
      $Basics.toString(n),
      "px");
   };
   var colorStr = function (c) {
      return function () {
         var $ = $Color.toRgb(c),
         red = $.red,
         green = $.green,
         blue = $.blue,
         alpha = $.alpha;
         return A2($Basics._op["++"],
         "rgba(",
         A2($Basics._op["++"],
         $Basics.toString(red),
         A2($Basics._op["++"],
         ",",
         A2($Basics._op["++"],
         $Basics.toString(green),
         A2($Basics._op["++"],
         ",",
         A2($Basics._op["++"],
         $Basics.toString(blue),
         A2($Basics._op["++"],
         ",",
         A2($Basics._op["++"],
         $Basics.toString(alpha),
         ")"))))))));
      }();
   };
   var tuply = $Native$IsomUtil.tuply;
   var translationalPart = function (t) {
      return function () {
         var _ = tuply(t);
         var x = function () {
            switch (_.ctor)
            {case "_Tuple6": return _._2;}
            _U.badCase($moduleName,
            "on line 65, column 48 to 55");
         }();
         var y = function () {
            switch (_.ctor)
            {case "_Tuple6": return _._5;}
            _U.badCase($moduleName,
            "on line 65, column 48 to 55");
         }();
         return {ctor: "_Tuple2"
                ,_0: x
                ,_1: y};
      }();
   };
   var invert = function (m) {
      return function () {
         var $ = tuply(m),
         a = $._0,
         b = $._1,
         x = $._2,
         c = $._3,
         d = $._4,
         y = $._5;
         var det = a * d - b * c;
         return _U.eq(det,
         0) ? $Debug.crash(A2($Basics._op["++"],
         "0 determinant: ",
         $Basics.toString(tuply(m)))) : function () {
            var s = 1 / det;
            return A6($Transform2D.matrix,
            s * d,
            (0 - s) * b,
            (0 - s) * c,
            s * a,
            0 - x,
            0 - y);
         }();
      }();
   };
   var distTransform2D = F2(function (trans,
   goal) {
      return function () {
         var $ = tuply(trans),
         t0 = $._0,
         t1 = $._1,
         t2 = $._2,
         t3 = $._3,
         t4 = $._4,
         t5 = $._5;
         var $ = tuply(goal),
         g0 = $._0,
         g1 = $._1,
         g2 = $._2,
         g3 = $._3,
         g4 = $._4,
         g5 = $._5;
         return $List.sum(A3($List.map2,
         F2(function (g,t) {
            return Math.pow(g - t,2);
         }),
         _L.fromArray([g0
                      ,g1
                      ,g2
                      ,g3
                      ,g4
                      ,g5]),
         _L.fromArray([t0
                      ,t1
                      ,t2
                      ,t3
                      ,t4
                      ,t5])));
      }();
   });
   var allTogether = function (_v28) {
      return function () {
         switch (_v28.ctor)
         {case "::": return function () {
                 var closeEnough = function (t2) {
                    return _U.cmp(A2(distTransform2D,
                    _v28._0,
                    t2),
                    1.0e-2) < 0;
                 };
                 return A2($List.all,
                 closeEnough,
                 _v28._1);
              }();}
         _U.badCase($moduleName,
         "between lines 129 and 130");
      }();
   };
   var isDiagonal = allTogether;
   var normalizeCirculan = function (r) {
      return function () {
         var $ = $Ratio.split(r),
         a = $._0,
         b = $._1;
         var a$ = A2($Basics._op["%"],
         a,
         b);
         var a$$ = _U.cmp(a$,
         (b / 2 | 0) + A2($Basics._op["%"],
         b,
         2)) > -1 ? a$ - b : a$;
         return A2($Ratio.over,a$$,b);
      }();
   };
   var modFloat = F2(function (x,
   m) {
      return x - m * $Basics.toFloat($Basics.floor(x / m));
   });
   var normalizeAngle = function (x) {
      return function () {
         var x$ = A2(modFloat,
         x,
         2 * $Basics.pi);
         return _U.cmp(x$,
         $Basics.pi) > 0 ? x$ - 2 * $Basics.pi : x$;
      }();
   };
   var and = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "::":
            return xs._0 && and(xs._1);
            case "[]": return true;}
         _U.badCase($moduleName,
         "between lines 42 and 44");
      }();
   };
   var maybe = F3(function (y,
   f,
   mx) {
      return function () {
         switch (mx.ctor)
         {case "Just": return f(mx._0);
            case "Nothing": return y;}
         _U.badCase($moduleName,
         "between lines 38 and 40");
      }();
   });
   var firstDo = F2(function (x,
   y) {
      return A2($Transform2D.multiply,
      y,
      x);
   });
   var filterFold = F3(function (f,
   z,
   s) {
      return $Signal.map($Basics.fst)(A2($Signal.filter,
      $Basics.snd,
      {ctor: "_Tuple2"
      ,_0: z
      ,_1: true})(A3($Signal.foldp,
      F2(function (a,_v37) {
         return function () {
            switch (_v37.ctor)
            {case "_Tuple2":
               return function () {
                    var _v41 = A2(f,a,_v37._0);
                    switch (_v41.ctor)
                    {case "Just":
                       return {ctor: "_Tuple2"
                              ,_0: _v41._0
                              ,_1: true};
                       case "Nothing":
                       return {ctor: "_Tuple2"
                              ,_0: _v37._0
                              ,_1: false};}
                    _U.badCase($moduleName,
                    "between lines 22 and 24");
                 }();}
            _U.badCase($moduleName,
            "between lines 22 and 24");
         }();
      }),
      {ctor: "_Tuple2"
      ,_0: z
      ,_1: true},
      s)));
   });
   var filterMap = F2(function (f,
   y) {
      return A2(filterFold,
      F2(function (x,_v43) {
         return function () {
            return f(x);
         }();
      }),
      y);
   });
   var filterJust = filterMap($Basics.identity);
   var sing = function (x) {
      return _L.fromArray([x]);
   };
   var styleNode = function (stys) {
      return function ($) {
         return A2($Html.node,
         "style",
         _L.fromArray([]))(sing($Html.text($)));
      }($String.join("\n")(A2($List.map,
      function (_v45) {
         return function () {
            switch (_v45.ctor)
            {case "_Tuple2":
               return A2($Basics._op["++"],
                 _v45._0,
                 A2($Basics._op["++"],
                 " ",
                 A2($Basics._op["++"],
                 "{",
                 A2($Basics._op["++"],
                 A2($String.join,
                 "\n",
                 A2($List.map,
                 function (_v49) {
                    return function () {
                       switch (_v49.ctor)
                       {case "_Tuple2":
                          return A2($Basics._op["++"],
                            _v49._0,
                            A2($Basics._op["++"],
                            ":",
                            A2($Basics._op["++"],
                            _v49._1,
                            ";")));}
                       _U.badCase($moduleName,
                       "on line 89, column 46 to 66");
                    }();
                 },
                 _v45._1)),
                 "}"))));}
            _U.badCase($moduleName,
            "between lines 88 and 90");
         }();
      },
      stys)));
   };
   _elm.Util.values = {_op: _op
                      ,sing: sing
                      ,filterFold: filterFold
                      ,filterMap: filterMap
                      ,filterJust: filterJust
                      ,firstDo: firstDo
                      ,maybe: maybe
                      ,and: and
                      ,modFloat: modFloat
                      ,normalizeAngle: normalizeAngle
                      ,normalizeCirculan: normalizeCirculan
                      ,tuply: tuply
                      ,translationalPart: translationalPart
                      ,invert: invert
                      ,distTransform2D: distTransform2D
                      ,styleNode: styleNode
                      ,colorStr: colorStr
                      ,px: px
                      ,onLastLevel: onLastLevel
                      ,listInit: listInit
                      ,signalMap6: signalMap6
                      ,signalMap7: signalMap7
                      ,splitAt: splitAt
                      ,groupsOf: groupsOf
                      ,allTogether: allTogether
                      ,isDiagonal: isDiagonal
                      ,foldr1: foldr1
                      ,foldl1: foldl1};
   return _elm.Util.values;
};
Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values)
   return _elm.VirtualDom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "VirtualDom",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var on = $Native$VirtualDom.on;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   _elm.VirtualDom.values = {_op: _op
                            ,Node: Node
                            ,node: node
                            ,text: text
                            ,toElement: toElement
                            ,fromElement: fromElement
                            ,Property: Property
                            ,property: property
                            ,attribute: attribute
                            ,on: on
                            ,lazy: lazy
                            ,lazy2: lazy2
                            ,lazy3: lazy3};
   return _elm.VirtualDom.values;
};
Elm.Window = Elm.Window || {};
Elm.Window.make = function (_elm) {
   "use strict";
   _elm.Window = _elm.Window || {};
   if (_elm.Window.values)
   return _elm.Window.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Window",
   $Basics = Elm.Basics.make(_elm),
   $Native$Window = Elm.Native.Window.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var dimensions = $Native$Window.dimensions;
   var width = A2($Signal.map,
   $Basics.fst,
   dimensions);
   var height = A2($Signal.map,
   $Basics.snd,
   dimensions);
   _elm.Window.values = {_op: _op
                        ,dimensions: dimensions
                        ,width: width
                        ,height: height};
   return _elm.Window.values;
};