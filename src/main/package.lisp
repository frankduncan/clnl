(defpackage #:clnl (:use :common-lisp)
 (:export
  #:run #:boot #:run-commands #:run-reporter #:*model-package*
  #:model->multi-form-lisp #:model->single-form-lisp)
 (:documentation
  "Main CLNL package

The entry point for general purpose clnl startup, as well as
the place that ties all the parts together into a cohesive whole."))

(defpackage #:clnl-parser
 (:use :common-lisp)
 (:export #:parse)
 (:documentation
  "CLNL Parser

All the code to convert the list of tokens coming from the lexer
into an ast that can be transpiled later."))

(defpackage #:clnl-code-parser
 (:use :common-lisp)
 (:export #:parse #:globals)
 (:documentation
  "CLNL Code Parser

A parser specifically for code from NetLogo models, that turns the lexed
ast from an entire structured file into something more defined.

This is different from the general parser (in clnl-parser) in that
it's made for parsing the code section of nlogo files, and so works
outside of the constraints.  In NetLogo, I believe this is analagous
to the StructureParser, but I'm guessing there's weird overlap with
other things."))

(defpackage #:clnl-random
 (:use :common-lisp)
 (:shadow #:export)
 (:export #:export #:set-seed #:next-int #:next-double #:next-long)
 (:documentation
  "Wrapper around mt19937.

mt19937 implements a merseinne twister that must be adapted a little in
order to match the implementation in the main NetLogo codebase which tries
to match how java.util.Random works.  Turtles, all the way down."))

(defpackage #:clnl-transpiler
 (:use :common-lisp)
 (:export #:transpile #:reporter-p #:command-list-p)
 (:documentation
  "CLNL Transpiler

The transpiler is responsible for taking an ast and turning it into valid CL code
targeting the nvm.  Here is where start to care about commands versus reporters
and ensuring that things are in the right place.  The reason we wait until here
is because we want to allow someone else to play with the AST before handing it off
to us.  For instance, the command center wants to add \"show\" to reporters, and
the users dictate based on entry point whether they are expecting a command
or a reporter.  So monitors can say \"hey, transpile this reporter\" and we'll check
to make sure it actually is.

Furthermore, the lisp code that any netlogo code would be transpiled to should
use exported symbols, such that anyone writing NetLogo code in lisp could use
the nvm in the same way that comes out of this transpiler
All the code to convert the list of tokens coming from the lexer
into an ast that can be transpiled later."))

(defpackage #:clnl-nvm
 (:use :common-lisp)
 (:shadow #:random #:count)
 (:export #:export-world #:create-world #:current-state #:with-stop-handler
  ; API as used by transpiled NetLogo programs
  #:agent-value
  #:ask
  #:clear-all
  #:count
  #:create-turtles
  #:die
  #:display
  #:hatch
  #:of
  #:forward
  #:lookup-color
  #:one-of
  #:patches
  #:reset-ticks
  #:random
  #:random-float
  #:random-xcor
  #:random-ycor
  #:set-default-shape
  #:setxy
  #:show
  #:stop
  #:turtles
  #:tick
  #:ticks
  #:turn-right #:turn-left
  #:with)
 (:documentation
  "CLNL NVM

NetLogo Virtual Machine: the simulation engine."))

(defpackage #:clnl-lexer
 (:use :common-lisp)
 (:export #:lex)
 (:documentation
  "CLNL Lexer

The primary code responsible for tokenizing NetLogo code."))

(defpackage #:clnl-interface
 (:use :common-lisp)
 (:export #:run #:export-view #:initialize)
 (:documentation
  "CLNL Interface

The NetLogo view interface using opengl.  This is responsible for taking the
current state of the enging and displaying it.  Will not house any interface
components."))

(defpackage #:clnl-cli
 (:use :common-lisp :cl-charms/low-level)
 (:export #:run)
 (:documentation
  "CLNL CLI

The main NetLogo interface for interacting with the program.  Since CLNL is
a command line interface program with a view for display purposes only, this
is where all the features that the traditional NetLogo UI lives."))

(defpackage #:clnl-model
 (:use :common-lisp)
 (:export #:default-model #:read-from-nlogo #:world-dimensions #:widget-globals #:code)
 (:documentation
  "CLNL Model

The representation, parsing, and serializing of NetLogo model files, including
all of the sections, and subsections held within.  This package houses not only
the code to read and write .nlogo files, but also the living state of the model
as clnl runs."))
