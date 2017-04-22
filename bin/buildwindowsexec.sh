#!/bin/bash

# This sciprt builds a version of sbcl pre loaded with libraries that can run on windows.
# You can use ./winsbcl.exe to run it

# It is meant to be run from cygwin

if ! type sbcl &> /dev/null ; then
  echo "Please install sbcl.  A version has been included in deps/windows for your convenience"
  exit 1
fi

sbcl_exec=$(which sbcl)
sbcl_dir=${sbcl_exec%sbcl}

if [ ! -e "$sbcl_dir/freeglut.dll" ] ; then
  echo "Please copy deps/windows/freeglut.dll into $sbcl_dir"
  exit 1
fi

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

cur_dir=$(cygpath -d $PWD)
escaped_cur_dir=${cur_dir//\\/\\\\}

sbcl --no-sysinit --no-userinit \
  --eval "(require 'asdf)" \
  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"${escaped_cur_dir}\\\\tmp\\\\deps\") (:tree \"${escaped_cur_dir}\\\\src\\\\main\") :IGNORE-INHERITED-CONFIGURATION))" \
  --eval "(asdf:load-system :clnl)" \
  --eval "(asdf:load-system :clnl-extension-cli)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "clnl.exe" :executable t :toplevel (function clnl:run))'

rm -rf tmp

echo "Executable is clnl.exe"
