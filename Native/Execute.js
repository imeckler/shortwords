/*
  The MIT License (MIT)
 
 Copyright (c) 2015, Paul Chiusano and respective contributors
 
 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
 5
*/
Elm.Native.Execute = {};

Elm.Native.Execute.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Execute = localRuntime.Native.Execute || {};
    if (localRuntime.Native.Execute.values) {
        return localRuntime.Native.Execute.values;
    }
    var Signal = Elm.Signal.make(localRuntime);
    
    // schedule : Signal Message -> Signal ()
    function schedule(msgs) {
        var tuple0 = { ctor: "_Tuple0" }; 
        function scheduleForce(thunk) {
          setTimeout(thunk, 0);
          return tuple0;
        }
        return A2( Signal.map, scheduleForce, msgs );
    }

    // complete : Signal Message -> Signal ()
    function complete(msgs) {
        var tuple0 = { ctor: "_Tuple0" }; 
        var output = Signal.constant(tuple0);
        // need to nest the calls to `setTimeout` to ensure the output signal
        // is refreshed *after* the message has been delivered
        function scheduleForce(thunk) {
          setTimeout(
            function() { 
              thunk(); 
              setTimeout(function() { localRuntime.notify(output.id, tuple0); }, 0)
            }, 0);
          return tuple0;
        }
        var forced = A2( Signal.map, scheduleForce, msgs );
        function k(x) { return function(y) { return x; } }
        // the sampleOn output is important, since the `map2` would otherwise 
        // emit an event when either a message comes in OR a message is delivered 
        return A2(Signal.sampleOn, output, A3( Signal.map2, k, output, forced));
    }

    return localRuntime.Native.Execute.values = {
        schedule: schedule,
        complete : complete
    };
};

