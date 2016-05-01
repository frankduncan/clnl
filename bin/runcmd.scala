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
workspace.openFromSource(url2String("file:resources/empty.nlogo"))

val input = io.Source.stdin.getLines.mkString("\n").split("\\@\\#\\$\\#\\@\\#\\$\\#\\@")
val commands = input(0)

workspace.mainRNG.setSeed(15)
if(commands.length > 0) {
  workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, classOf[api.Observer]), workspace.compileCommands(commands))
}
if(input.length > 1) {
  val reporter = input(1)
  System.out.println(org.nlogo.api.Dump.logoObject(workspace.runCompiledReporter(new api.SimpleJobOwner("test", workspace.world.mainRNG, classOf[api.Observer]), workspace.compileReporter(reporter))))
}

workspace.world.exportWorld(new java.io.PrintWriter(System.out, true), true)
System.out.println(org.nlogo.headless.Checksummer.calculateChecksum(workspace.world.exportWorld(_, true)))

workspace.dispose
