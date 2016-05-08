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

val input = io.Source.stdin.getLines.mkString("\n").split("\\@\\#\\$\\#\\@\\#\\$\\#\\@")

System.out.println("----")
val workspace = HeadlessWorkspace.newInstance
workspace.silent = true

if (input.length > 2 && input(2).length > 0) {
  val modelSetup = input(2)
  workspace.openFromSource(modelSetup +
"""
@#$#@#$#@
GRAPHICS-WINDOW
210
10
649
470
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-1
1
-1
1
0
0
1
ticks
30.0

@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
NetLogo 5.2.0""")
} else {
  workspace.openFromSource(url2String("file:resources/empty.nlogo"))
}

workspace.mainRNG.setSeed(15)

val commands = input(0)
if(commands.length > 0) {
  workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, classOf[api.Observer]), workspace.compileCommands(commands))
}
if(input.length > 1) {
  val reporter = input(1)
  if(reporter.length > 0) {
    System.out.println(org.nlogo.api.Dump.logoObject(workspace.runCompiledReporter(new api.SimpleJobOwner("test", workspace.world.mainRNG, classOf[api.Observer]), workspace.compileReporter(reporter))))
  }
}

workspace.world.exportWorld(new java.io.PrintWriter(System.out, true), true)
System.out.println(org.nlogo.headless.Checksummer.calculateChecksum(workspace.world.exportWorld(_, true)))

workspace.dispose
