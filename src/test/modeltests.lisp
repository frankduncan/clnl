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
