#!/bin/bash

# This sciprt builds a version of sbcl pre loaded with libraries that can run on osx.
# You can use ./osxsbcl --script bin/run.lisp to run it

mkdir -p tmp/sbcl
mkdir -p tmp/sbcl-binary

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/osx/sbcl-1.2.11-x86-64-darwin-binary.tar.bz2 &&
  tar jxf ../deps/osx/sbcl-1.2.13-source.tar.bz2 &&
  cd sbcl-1.2.11-x86-64-darwin/ &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl-binary/ bash install.sh &&
  cd ../sbcl-1.2.13/ &&
  PATH="$cwd/tmp/sbcl-binary/bin/:$PATH" SBCL_HOME="$cwd/tmp/sbcl-binary/lib/sbcl/" bash make.sh --with-sb-thread &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl/ bash install.sh )

mkdir -p tmp/deps/

( cd tmp/deps &&
  tar zxf ../../deps/common-lisp/clnl-gltk_0.1.tar.gz &&
  tar zxf ../../deps/common-lisp/3b-cl-opengl-993d627.tar.gz &&
  tar zxf ../../deps/common-lisp/alexandria-b1c6ee0.tar.gz &&
  tar zxf ../../deps/common-lisp/babel_0.5.0.tar.gz &&
  tar zxf ../../deps/common-lisp/cffi_0.15.0.tar.gz &&
  tar zxf ../../deps/common-lisp/cl-ppcre.tar.gz &&
  tar zxf ../../deps/common-lisp/ironclad.tar.gz &&
  tar zxf ../../deps/common-lisp/mt19937-latest.tar.gz &&
  tar zxf ../../deps/common-lisp/nibbles-v0.12.tar.gz &&
  tar zxf ../../deps/common-lisp/trivial-features_0.8.tar.gz &&
  tar zxf ../../deps/common-lisp/ieee-floats-92e481a.tar.gz &&
  tar zxf ../../deps/common-lisp/strictmath_0.1.tar.gz
)

SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core --no-sysinit --no-userinit \
  --eval "(require 'asdf)" \
  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"${PWD}/tmp/deps\") (:tree \"${PWD}/src/main\") :IGNORE-INHERITED-CONFIGURATION))" \
  --eval "(asdf:load-system :clnl)" \
  --eval "(asdf:load-system :clnl-extension-cli)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "osxsbcl" :executable t :toplevel (function clnl:run))' \

chmod +x osxsbcl

rm -rf tmp
