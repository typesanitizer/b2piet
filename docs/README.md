# b2piet: A Brainfuck to Piet transpiler

## What :open_mouth:

## Hi :smile:

Pick your favourite tagline:

> Fast, correct and beautiful - pick 3.

> Fucking drawings.

> Ultrapostmodern polyfunctional cybergraffiti.

> :arrow_forward::fu::computer::tractor::no_good::art::computer::rocket::metal:

## Build :cold_sweat:

### Pre-requisites

#### Platforms

The program has only been tested on Linux.
Ideally, it should work on OS X as is.
It will probably not work on Windows.

#### Dependencies

By default, `b2piet` will only be able to produce `.ppm` and `.bmp` images.
For very large images, the file size will quickly become large if you use `.ppm`
files.
So it is strongly suggested that you install an external library to be able to
save images as `.png` files. The appropriate dependency name can be found
in the `camlimages`'s installation instructions [here][camlimages-install].

It is currently listed as `libpng`, which you can install through your
distribution's package manager.
You should install the Ocaml package `camlimages` (described below) _after_
you've installed `libpng`.

You will need a working installation of Ocaml version
`v : 4.03.0 ≤ v ≤ 4.04.2`.
Versions outside this range may or may not work.
Instructions for installing Ocaml can be found on [ocaml.org][ocaml].

Don't forget to double-check the installed version number before proceeding
to install packages.

You should install the following dependencies using [OPAM][opam]
(OPAM itself will be installed when you install Ocaml):

* `atdgen`
* `batteries`
* `camlimages`
* `cmdliner`
* `integers`
* `ocamlfind`
* `ocamlbuild`
* `qcheck` (for tests)
* `ppx_deriving` (latest version)
* `merlin` (optional, latest version)

These can usually be installed as:

```
# Install libpng using your package manager
opam install atdgen batteries camlimages cmdliner integers ocamlfind ocamlbuild qcheck
opam pin add ppx_deriving --dev-repo
opam pin add merlin --dev-repo
```

Lastly, you will need some version of `make` (e.g. `GNU make`) for ease of use.

[ocaml]: http://ocaml.org/
[opam]: https://opam.ocaml.org/
[camlimages-install]: https://bitbucket.org/camlspotter/camlimages/src/b18e82a3d840c458f5db3f33309dd2d6e97bef91/INSTALL.rst?at=default

## How :sunglasses:

## Thanks :clap:

Thanks to all the developers of our dependencies. Special thanks to authors of
`cmdliner` and `qcheck` for having the best documentation :smile:.
