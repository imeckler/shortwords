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
