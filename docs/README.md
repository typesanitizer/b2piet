# b2piet: A Brainfuck to Piet transpiler

## What :open_mouth:

`b2piet` is an optimising [Brainfuck][Brainfuck] to [Piet][Piet]
[transpiler][transpiler] written in [Ocaml][Ocaml].

Currently one painting style is supported:

### Linear

The following program prints a Sierpinski triangle to depth 5 (converted from
Daniel Cristofani's [program][Sierpinski-b]):

!["Sierpinski triangles in Piet"][Sierpinski-Piet]

You can test it online using [npiet][npiet-online].

You can also check out this [animation][Tower-of-Hanoi-animation] of running
Towers of Hanoi (converted from Clifford Wolf's [code][Hanoi-b]).

(I can't share the generated program as Imgur won't let me upload a 131080x44
image, even though it is only 80 kB in size. If you do generate it locally,
it might be fun to test which image viewers you can crash with it.
For me, GIMP and Shotwell worked fine whereas Pinta and Eye of Gnome crashed.)

### [WIP] Mondrian

This will mimic [Piet Mondrian][PM]'s [tableau][tableau]. The colouring part
is not yet ready, so I don't have anything to show here...

[Brainfuck]: http://www.muppetlabs.com/~breadbox/bf/
[Piet]: http://www.dangermouse.net/esoteric/piet.html
[transpiler]: https://en.wikipedia.org/wiki/Source-to-source_compiler
[Ocaml]: https://ocaml.org/
[PM]: https://en.wikipedia.org/wiki/Piet_Mondrian
[tableau]:
https://duckduckgo.com/?q=Piet+Mondrian+tableau&t=hb&iar=images&iax=1&ia=imagesL
[Sierpinski-b]: http://www.hevanet.com/cristofd/brainfuck/sierpinski.b
[Sierpinski-Piet]: https://i.imgur.com/04AXBFf.png
[Tower-of-Hanoi-animation]: https://i.imgur.com/kaMxr6y.gifv
[Hanoi-b]: http://www.clifford.at/bfcpu/hanoi.html

## Thanks :clap:

Thanks to all the developers of our dependencies,
and folks answering questions on StackOverflow, IRC and Reddit.
Special thanks to the authors of Cmdliner and Qcheck for having especially nice
documentation :smile:,
[@aantron](https://github.com/aantron) for helping me out with Lwt,
and to the authors of Merlin,
which makes programming in Ocaml a pleasure :heart_eyes:.

Lastly, thanks to the authors of [Real World Ocaml][real-world-ocaml] for
distributing their book freely online.

[real-world-ocaml]: https://realworldocaml.org/v1/en/html/index.html

## Start :smile:

Pick your favourite tagline:

> Fast, correct and beautiful - pick 3.

> Postmodern multifunctional cybergraffiti.

> Unattractive Turing completeness considered harmful. (sorry)

## Build :cold_sweat:

### Pre-requisites

#### Platforms

The program has only been tested on Ubuntu 16.04 and 17.04.
_Ideally_, it should work on other GNU/Linux distributions,
*BSD and macOS as is.
It will probably not work on Windows.

See [Issues](#issues-sob) if you want support added for your platform.

#### Dependencies

By default, `b2piet` will only be able to produce `.ppm` and `.bmp` images.
For very large images, the file size will quickly become large if you use `.ppm`
files.
So it is _strongly suggested_ that you install an external library to be able to
save images as `.png` files. The appropriate dependency name can be found
in the `camlimages`'s installation instructions [here][camlimages-install].
This dependency is currently listed as `libpng`, which you can install through
your distribution's package manager.
You should install the Ocaml package `camlimages` (described below) _after_
you've installed `libpng`.

You will need a working installation of Ocaml version
`v : 4.03.0 ≤ v ≤ 4.05.0`.
Versions outside this range may not work (haven't tested yet).
Instructions for installing Ocaml can be found on [ocaml.org][ocaml].
Don't forget to double-check the installed version number (`ocamlc --version`)
before proceeding to install packages.

You should install the following dependencies using [OPAM][opam]
(OPAM itself will be installed when you install Ocaml):

* `atdgen`
* `batteries`
* `camlimages`
* `cmdliner`
* `integers`
* `lwt`
* `ppx_deriving`
* `qcheck` (for tests)

These can be installed as:

```
# Install libpng using your package manager
opam install atdgen batteries camlimages cmdliner integers lwt qcheck ppx_deriving
```

You will also need `fpiet` if you want to run tests (see [Testing output](#testing-output)).
The `fpiet` binary should be placed in the project's root directory.

Lastly, you will need some version of `make` (e.g. `GNU make`) for ease of use.

[ocaml]: http://ocaml.org/
[opam]: https://opam.ocaml.org/
[camlimages-install]: https://bitbucket.org/camlspotter/camlimages/src/b18e82a3d840c458f5db3f33309dd2d6e97bef91/INSTALL.rst?at=default

### Building `b2piet`

Clone this repository, `cd` into the directory and run `make`.

```
git clone https://github.com/theindigamer/b2piet.git
cd b2piet
make
```

This will create a file `b2piet.byte` which you can use.
If you want a native file too (runs much faster but slower to compile),
run `make all` instead of `make`.

## Usage

Run `./b2piet.byte --help` or `./b2piet.native --help` to see full descriptions
for the commandline flags.
Usage examples are given near the end, you might want to read those first and
get back to the flag descriptions later in case you need them.

### Testing output

For simple programs, you can test the Piet output online using Erik
Schoenfelder's interpreter [npiet][npiet-online].

Larger programs will need a lot more steps than the website supports,
so it is suggested that you have a Piet interpreter locally and run it in the
terminal.
Matthias Ernst's [`fpiet`][fpiet] works well; it is much faster then `npiet`.

Once you have a binary for `fpiet`, you can check the output using
`./fpiet a.png` (similarly for `npiet`).

**Note:** `fpiet` does not work with the `.ppm` files generated by `b2piet`
as the former accepts ASCII ppm (P3) whereas the latter emits binary ppm (P6).
If you insist on using `.ppm` files, you can use `npiet`
(either online or offline) which accepts binary ppm,
or you can add a conversion step (note: requires [`imagemagick`][imagemagick])
before running `fpiet`:

```
./b2piet.byte --output=foo.ppm foo.b
convert foo.ppm foo.png
./fpiet foo.png   # works
# ./fpiet foo.ppm # doesn't work
# ./npiet foo.ppm # works
```

[npiet-online]: http://www.bertnase.de/npiet/npiet-execute.php
[fpiet]: http://www.matthias-ernst.eu/fpiet.html
[imagemagick]: https://www.imagemagick.org/script/download.php

## Issues :sob:

**NOTE:**
Before opening an issue, please search if the same issue was reported earlier.
By default, Github only searches through open issues. However,
you should double-check closed issues as well before submitting a new one.

#### I want support added for platform X.

→ Open an issue on the issue tracker.

#### I have a feature request or some general comments.

→ Open an issue on the issue tracker.

#### I am unable to build `b2piet` despite following all the instructions.

→ Open an issue on the issue tracker.

#### `b2piet` unexpectedly crashes for non-malicious input.

→ Submit a [bug report](#bug-reports) on the issue tracker.

#### The generated Piet program does not work as expected.

If it makes sense to use the `--stack-auto` flag (see `./b2piet.byte --help`),
then try doing that and check the output.
If it still doesn't work,
try using larger stack sizes manually up to the limit of 30000.
If it still doesn't work, or runs way longer than is expected
(this is likely to happen when the stack size is very large),
please submit a [bug report](#bug-reports) on the issue tracker.

#### `b2piet`'s output doesn't seem to follow the painting style specified.

→ Submit a [bug report](#bug-reports) on the issue tracker.

#### This repository uses way too many emojis.

++++++++[>+++++++>+++++++++++++>+++++++++++++++<<<-]>++.>--.>---.<<.<.

#### My issue isn't covered by the aforementioned cases.

No worries, the issue tracker can handle meta-issues as well.

### Bug reports

Fill out the issue template -- you will see this when you create a new issue --
as appropriate.

## How :sunglasses:

### do I contribute?

See [Contributing](contributing.md).

### does this work?

* [design.md](design.md) gives a rough description of the internals.
* [tableau.md](tableau.md) describes the overall layout for tableau.
* [loops.org](loops.org) shows how arbitrary loops are constructed.
* See comments in the source code or generate documentation from documentation
  comments using `make doc` and opening `b2piet.docdir/index.html` in your
  browser.

## Trivia

* I found a small
  [bug](https://github.com/ocaml-batteries-team/batteries-included/issues/766)
  in the `batteries-included` standard library while working on this project.
  Yay!

## Frequently Unasked Questions

#### Why are you using `.b` file extensions instead of `.bf`?

Um, didn't you learn the following in school?

> Cain is for Charlie, and Delta is for Cain,
>
> and BravoFoxtrot is for Befunge, and Bravo is for Brainfuck.

#### What are the possible use cases for this wonderful program?

Possible passive use cases include but are not restricted to:

* Your device wallpaper, to remind yourself how nerdy you are.
* A poster inside your house to wow your imaginary guests.
* A fancy nameplate outside your house to throw off your real postman.

Possible active use cases include:

* Smart wallpaper that records conversations.
* Beautiful Skynet.
* Anything you can think of! After all, Brainfuck is Turing complete :smile:.

```
 _____________________
< Thanks for reading! >
 ---------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
             n ||----w |
                ||     ||
```
