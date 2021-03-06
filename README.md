# clnl

This is an experiment at creating an alternate implementation of NetLogo in Common Lisp.

See the [wiki](https://github.com/frankduncan/clnl/wiki) for more information.

# Using standalone executables

Find the latest version, [0.1.0](https://github.com/frankduncan/clnl/releases/tag/v0.1.0) on the [releases page](https://github.com/frankduncan/clnl/releases):
* [linux](https://github.com/frankduncan/clnl/releases/download/v0.1.0/clnl)
* [windows](https://github.com/frankduncan/clnl/releases/download/v0.1.0/clnl.exe)
* [mac](https://github.com/frankduncan/clnl/releases/download/v0.1.0/CLNL.dmg)

# Running from source

If you'd like to run it from source, you're going to need a few things:

* A copy of the clnl source (either from the [releases page](https://github.com/frankduncan/clnl/releases), version [0.1.0](https://github.com/frankduncan/clnl/releases/download/v0.1.0/clnl_0.1.0.tar.gz) or via cloning)
* An implementation of sbcl with threads enabled
* The following common lisp libraries (included in [deps/common-lisp](deps/common-lisp) folder)
  * alexandria
  * babel
  * cffi
  * clnl-gltk
  * cl-opengl
  * cl-ppcre
  * ironclad
  * mt19937
  * nibbles
  * trivial-features
  * strictmath
  * ieee-floats

[bin/nl](bin/nl) and [bin/run.lisp](bin/run.lisp) have been added for convenience to run the netlogo instance.  Not very many commands are implemented, but it should alert you to that.  A good test is

```
crt 10
ask turtles [ fd 1 ]
show random 5
ask turtles [ fd 5 ]
```

## Running on OSX

In order to run on OSX, you may have to build your own sbcl instance with threads enabled.  See [bin/buildosxexec.sh](bin/buildosxexec.sh) for how it's done when releasing/testing.

## Running on Windows

In order to run on Windows, you will need to install a copy of 32bit sbcl with threads enabled, as well as putting a copy of freeglut in that directory.  See [bin/buildwindowsexec.sh](bin/buildwindowsexec.sh) for how it's done when releasing/testing.

# Running in a common lisp instance

If you'd like to run using your own sbcl instance, you can attach the clnl.asd file wherever you link asd files, and then use:

```lisp
(asdf:load-system :clnl)
(clnl:run)
```
