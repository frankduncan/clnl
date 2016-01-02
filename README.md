# clnl

[![Join the chat at https://gitter.im/frankduncan/clnl](https://badges.gitter.im/frankduncan/clnl.svg)](https://gitter.im/frankduncan/clnl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is an experiment at creating an alternate implementation of NetLogo mainly as an exercise for me so that I come to understand NetLogo better.  It is true for me that I only truly understand something when I can explain it to someone else without repetition.  In code, this manifests as expressing a program in another language.

See the [wiki](https://github.com/frankduncan/clnl/wiki) for more information at this time.

# Building

If you'd like to build it, you're going to need a few things:

* An implementation of sbcl with threads enabled
* The following common lisp libraries (included in deps/tarpit folder)
  * alexandria
  * babel
  * cffi
  * cl-charms
  * cl-opengl
  * cl-ppcre
  * docgen
  * ironclad
  * mt19937
  * nibbles
  * trivial-features
  * style-checker
* rlwrap

# Running as a program

bin/nl has been added for convenience to run the netlogo instance.  It boots up the ncurses command line with an opengl view.  Not very many commands are implemented, but it should alert you to that.  A good test is

```
crt 10
ask turtles [ fd 1 ]
show random 5
ask turtles [ fd 5 ]
```

# Running in a common lisp instance

If you'd like to run using your own sbcl instance, you can attach the clnl.asd file wherever you link asd files, and then use:

```lisp
(asdf:load-system :clnl)
(clnl:run)
```

## Running on OSX

In order to run on OSX, you may have to build your own sbcl instance with threads enabled.

# Testing

If you'd like to see the tests go on your system, there's a number of utilities to look at:

* bin/test.lisp
* bin/diagnose-test
* bin/diagnose-view-test
* bin/test-mode
