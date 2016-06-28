(in-package #:clnl-test)

(defmodelcommandtest "globals 1"
 "globals [a]"
 "set a 5 crt a"
 "4D66EDE80A8F4CA820D80853E763446502EA4E4E")

(defmodelreportertest "globals 2"
 "globals [a]"
 "set a 5"
 "a"
 "5"
 "F8507A0D88D681CCBF01898FEA263791F9DDCE63")

(defmodelcommandtest "to 1"
 "globals [a]
to setup
  set a 2
end

to go
  crt a
end"
 "setup go go"
 "1A20E368DD101521791FB5D7C8461C1ED12EAE7A")

(defmodelcommandtest "to 2"
 "globals [a]
to setup
  set a 2
end

to go
  setup
  crt a
end"
 "go"
 "46C620AB8995266C4A2094C461BE197BBACEB8C3")

(defmodelcommandtest "turtles-own 1"
 "turtles-own [a b]
to setup
  crt 10 [
    set a 2
  ]
end"
 "setup"
 "482947557971AC2A66CB35AA5D6850A489C45215")

(defmodelcommandtest "turtles-own 2"
 "turtles-own [a b]
to setup
  crt 10 [
    set a 2
    set b a + 1
  ]
end

to go
  ask turtles [ fd b ]
end"
 "setup go"
 "F8A2BFD71A8A064C37DDB744217AB07CDB0686EB")

(defmodelcommandtest "patches-own 1"
 "patches-own [a]
to setup
  ask patches [
    set a 2
  ]
end"
 "setup"
 "73FE87B52A2DAB0EC02DB23F26DB3B5336A61679")

(defmodelcommandtest "patches-own 2"
 "patches-own [a b]
to setup
  ask patches [
    set a 2
    set b a + 1
  ]
end

to go
  ask patches [ set pcolor b ]
end"
 "setup go"
 "2972B3EC1285BDA17656401001E1AE667FA7F5AF")

(defmodelcommandtest "breeds 1"
 "breed [wolves wolf]

to setup
  create-turtles 50
  create-turtles 50 [ fd 1 ]
  create-wolves 50
  set-default-shape wolves \"sheep\"
  create-wolves 50 [ fd 1 ]
end

to go
  ask turtles [ fd 1 ]
  ask wolves [ fd 1 ]
  ask turtles [ if 1 < count turtles-here [ fd 1 ] ]
  ask wolves [ if 1 < count turtles-here [ fd 1 ] ]
  ask turtles [ if 1 < count wolves-here [ fd 1 ] ]
  ask wolves [ if 1 < count wolves-here [ fd 1 ] ]
end"
 "setup go"
 "2614B99F64ACFA2BD64D66B129C0A17F2150FADD")

(defmodelcommandtest "procedures stop"
 "to setup
  create-turtles 5
  stop
  create-turtles 5
end

to go
  if 5 < count turtles [ stop ]
  crt 1
end"
 "setup go go"
 "438848EF35C6B0D28D50961072C70FCC02BB4FD8")

(defmodelfiletest "Wolf Sheep 1" "resources/models/Wolf Sheep Predation.nlogo"
 "setup go go go go go go go go go go go go go go"
 "9777CCF18935E52D8380C9C6DC02BFFBEE1F1149")

(defmodelfiletest "Wolf Sheep 2" "resources/models/Wolf Sheep Predation.nlogo"
 "set grass? not grass? setup go go go go go go go go go go go go go go"
 "FC38F01DC0058C5EFF93F2228535ED7C616ECFF0")

(defmodelfiletest "Fire 1" "resources/models/Fire.nlogo"
 "setup go go go go go go go go go go go go go go"
 "EDFD37E1EDC7B32499040274980756B68897F9FA")

