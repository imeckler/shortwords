# Short Words

I recently demoed my Elm library ["stage"](http://package.elm-lang.org/packages/imeckler/stage/1.3.0)
for building up piecewise functions of time, typically to make animations.

[Short Words](http://www.izaakmeckler.com/pages/shortwords/shortwords.html)
is a game.
There's currently only one (short) level currently and things are a bit rough around
the edges. The goal is to move the blue "R" to match the red "R" using the available
moves. The number of moves you have remaining is shown at the top left.

The word "word" in the title "Short Words" refers to
["word" in group theory](http://en.wikipedia.org/wiki/Word_%28group_theory%29).
The idea is that in the game you're given a particular set of elements
of the isometry of group (i.e., the transformations performable using the buttons)
and you have to construct a short word representing the isometry given by the
position/orientation of the red "R".
(Strictly speaking it's more like a word in a monoid since I don't give you inverses.)
