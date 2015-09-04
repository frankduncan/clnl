#!/bin/bash

bin/buildosxexec.sh && \
  mkdir tmp && \
  cp -ap dist/osx/CLNL.app tmp && \
  mv osxsbcl tmp/CLNL.app/Contents/MacOS/ && \
  cd tmp && \
  tar zxf ../deps/osx/create-dmg-5acf22f.tar.gz && \
  cd create-dmg && \
  cp ../CLNL.app/Contents/Resources/CLNL.icns . && \
  ./create-dmg --volname "CLNL Installer" --volicon "CLNL.icns" --icon-size 100 --window-size 250 250 --icon CLNL.app 150 100 CLNL.dmg ../CLNL.app && \
  mv CLNL.dmg ../.. && \
  cd ../.. && \
  rm -rf tmp
