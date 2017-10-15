# Piet Mondrian's Tableau

Thomas Schoch's program
!["Piet"](http://www.dangermouse.net/esoteric/piet/Piet-4.gif)
serves as a good starting point on how one might create arbitrary programs as
tableau.
The program is broken up into coloured rectangles (panels) which are separated
by solid (well, almost solid) black lines (rules).
One of the basic ideas is to have "tunnels" in the rules so that the program
can flow from one coloured (possibly white) panel to another.

In our case, unlike the program shown above, we will stick to using red, blue,
yellow, black and white for filling the different panels for authenticity.

Overall tableau generation

```
| > |   |   |   |   |   |   |   |   |   |   |   |  >|               |   |v  |  >|   |   |   |   |v  |v  |   ---#
|   |   |   |   +---|---|---|---+   +---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|   |      |
|   |   |   |   |      red      |   |           |   |  v|   |   |   |   |<  |  ^|   |   |   |   |<  |   |      |
|---|---|---+   +---|---|---|---+   |           +   +---|---|---|---|---|---|---|---|---|---|---|---|   |      |-- 1 row
| blue  |   |   |   |       | k |   |           |   |  >|   |   |   |   |v  |   |   |               |   |      |
|---|---+   +---|---|---|---|---+   |           +   +---|---|---|---|---|---|---|   |               |   |      |
|       |   |   |     yellow    |   |           |  ^|   |   |   |   |   |<  |   |   |               |   |   ---#
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|                                                 .....                                             |<  |
```

Panels cutting across rows MUST be white. This is to prevent control flow from
jumping across rows.

```
|   |   |   |   |   |   |v  |   ---#
|   |   |   +---|---+   |   |      |-- row 1
|   |   |   |       |   |   |   ---#
|---|---|---+       +---|---|
|   |   |   | white |   |<  |   ---#
|   |   |   |       |   |   |      |
|   |   |   |       |   |   |      |-- row 2
|   |   |   +---|---+   |   |      |
```

How do we go about colouring the tableau?
