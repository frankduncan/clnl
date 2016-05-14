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
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

@#$#@#$#@
NetLogo 5.2.0""")
} else if (input.length > 3 && input(3).length > 0) {
  workspace.openFromSource(url2String("file:" + input(3)))
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
