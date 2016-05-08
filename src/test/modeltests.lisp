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
