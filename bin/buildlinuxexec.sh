#!/bin/bash

# This script builds a verion of sbcl with the libraries pre loaded
# for ease of travis.  Remember to update bin/buildosxsbcl when you
# update this.

mkdir -p tmp/sbcl

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/linux/sbcl-1.2.6-x86-64-linux-binary.tar.bz2 &&
  cd sbcl-1.2.6-x86-64-linux/ &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl/ bash install.sh )

mkdir -p tmp/deps/

( cd tmp/deps &&
  tar zxf ../../deps/common-lisp/3b-cl-opengl-993d627.tar.gz &&
  tar zxf ../../deps/common-lisp/alexandria-b1c6ee0.tar.gz &&
  tar zxf ../../deps/common-lisp/babel_0.5.0.tar.gz &&
  tar zxf ../../deps/common-lisp/cffi_0.15.0.tar.gz &&
  tar zxf ../../deps/common-lisp/cl-ppcre.tar.gz &&
  tar zxf ../../deps/common-lisp/ironclad.tar.gz &&
  tar zxf ../../deps/common-lisp/mt19937-latest.tar.gz &&
  tar zxf ../../deps/common-lisp/nibbles-v0.12.tar.gz &&
  tar zxf ../../deps/common-lisp/trivial-features_0.8.tar.gz &&
  tar zxf ../../deps/common-lisp/cl-charms-9bb94ef.tar.gz &&
  tar zxf ../../deps/common-lisp/ieee-floats-92e481a.tar.gz &&
  tar zxf ../../deps/common-lisp/strictmath_0.1.tar.gz
)

SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core \
  --eval "(require 'asdf)" \
  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"${PWD}/tmp/deps\") (:directory \"${PWD}/src/main\") :IGNORE-INHERITED-CONFIGURATION))" \
  --eval "(asdf:load-system :clnl)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "clnl" :executable t :toplevel (function clnl:run))'

chmod +x clnl

rm -rf tmp
