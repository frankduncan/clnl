#!/bin/sh
exec scalas "$0" -q "$@"
!#

/***
  logLevel := Level.Error

  logLevel in Global := Level.Error

  scalaVersion := "2.10.3"

  libraryDependencies ++= Seq(
    "asm" % "asm-all" % "3.3.1",
    "org.picocontainer" % "picocontainer" % "2.13.6",
    "org.nlogo" % "NetLogoHeadless" % "6.0.0-M3" from "http://ccl.northwestern.edu/devel/6.0.0-M3/NetLogoHeadless.jar"
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
workspace.openFromSource(url2String("file:resources/empty.nlogo"))

val input = io.Source.stdin.getLines.mkString("\n").split("\\@\\#\\$\\#\\@\\#\\$\\#\\@")
val commands = input(0)

workspace.mainRNG.setSeed(15)
if(commands.length > 0) {
  workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileCommands(commands, api.AgentKind.Observer))
}
if(input.length > 1) {
  val reporter = input(1)
  System.out.println(org.nlogo.api.Dump.logoObject(workspace.runCompiledReporter(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileReporter(reporter))))
}

workspace.world.exportWorld(new java.io.PrintWriter(System.out, true), true)
System.out.println(org.nlogo.headless.Checksummer.calculateChecksum(workspace.world.exportWorld(_, true)))

workspace.dispose
