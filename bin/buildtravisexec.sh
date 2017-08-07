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
  tar zxf ../../deps/common-lisp/style-checker_0.1.tar.gz &&
  tar zxf ../../deps/common-lisp/clnl-gltk_0.2.tar.gz &&
  tar zxf ../../deps/common-lisp/docgen_0.3.tar.gz &&
  tar zxf ../../deps/common-lisp/ieee-floats-92e481a.tar.gz &&
  tar zxf ../../deps/common-lisp/strictmath_0.1.tar.gz
)

SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core \
  --eval "(require 'asdf)" \
  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"${PWD}/tmp/deps\") :IGNORE-INHERITED-CONFIGURATION))" \
  --eval "(require 'asdf)" \
  --eval "(asdf:load-system :cl-ppcre)" \
  --eval "(asdf:load-system :mt19937)" \
  --eval "(asdf:load-system :ironclad)" \
  --eval "(asdf:load-system :clnl-gltk)" \
  --eval "(asdf:load-system :cl-opengl)" \
  --eval "(asdf:load-system :cl-glut)" \
  --eval "(asdf:load-system :style-checker)" \
  --eval "(asdf:load-system :docgen)" \
  --eval "(asdf:load-system :strictmath)" \
  --eval "(asdf:clear-output-translations)" \
  --eval "(push :travis *features*)" \
  --eval '(sb-ext:save-lisp-and-die "travissbcl" :executable t)' \

chmod +x travissbcl
travisname=travissbcl-$(git rev-parse --short HEAD)
mv travissbcl $travisname

echo "You should upload via the command: scp $travisname nami:/var/travis/sbcls/clnl/"
echo "You should also set travisname in .travis.yml to $travisname"

rm -rf tmp
