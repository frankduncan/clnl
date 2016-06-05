#!/bin/bash

# The release process:
#  * Change version number in clnl.asd
#  * Change where the README.md points to releases
#  * Run bin/generatedocs.sh to update wiki
#  * Create linux release using bin/buildlinuxexech.sh on a linux machine
#  * Create osx release using bin/buildosxrelease.sh on a mac and upload
#  * Create windows release using bin/buildwindowsexec.sh on a windows box and upload
#  * Make a release commit and push up, most likely a rebase of all the commits to make the previous steps work correctly
#  * Run this script to create the source tarball
#  * Create release on github (that should create the tag), use v<MAJOR>.<MINOR>.<PATCH> as the versioning scheme
#  * Upload the tar.gz as an extra file
#  * Set the tag in wiki milestones, update Running wiki page to point to new release

version=$(sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --eval '(format t "~A" (asdf:component-version (asdf:find-system :clnl)))' --eval "(quit)")

echo -n "Building version $version, hit enter to continue"
read

mkdir clnl_$version
cp -ap src/main/* clnl_$version/
tar zcf clnl_${version}.tar.gz clnl_$version/
rm -rf clnl_$version

echo "All done, it's in clnl_${version}.tar.gz, you should tag it and push it up to github"
echo "You should also build on OSX, Windows, and Linux using the various release scripts and push those up too."
