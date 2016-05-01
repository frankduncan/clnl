#!/bin/sh
exec scalas "$0" -q "$@"
!#

/***
  logLevel := Level.Error

  logLevel in Global := Level.Error

  scalaVersion := "2.9.2"

  libraryDependencies ++= Seq(
    "asm" % "asm-all" % "3.3.1",
    "org.picocontainer" % "picocontainer" % "2.13.6",
    "org.nlogo" % "NetLogo" % "5.2.0" from "http://ccl.northwestern.edu/netlogo/5.2.0/NetLogo.jar"
  )
*/

import org.nlogo.headless.HeadlessWorkspace
import org.nlogo.api
import org.nlogo.nvm
import org.nlogo.util.Utils.url2String

import collection.JavaConversions._

System.out.println("----")
val workspace = HeadlessWorkspace.newInstance
workspace.silent = true
workspace.openFromSource(url2String("file:resources/empty55.nlogo"))

val commands = io.Source.stdin.getLines.mkString("\n")

workspace.mainRNG.setSeed(15)
workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, classOf[api.Observer]), workspace.compileCommands(commands))

workspace.exportView("scala.png", "PNG")

workspace.dispose
