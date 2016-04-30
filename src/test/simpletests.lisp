(in-package #:clnl-test)

(defsimplecommandtest "Nothing" ""
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplecommandtest "Simple crt" "crt 1"
 "2F08B31AC06C9D5339E6B3E953C2B4B71FDB9CDE")

(defsimplecommandtest "Simple crt 2" "crt 5"
 "9FE588C2749CD9CE66CB0EA451EFB80476E881FB")

(defsimplecommandtest "Simple crt and fd random" "crt 30 ask turtles [ fd random-float 1 ]"
 "DED34D1D6492244E9E3813DE8DBF258F96636879")

(defsimplecommandtest "Simple crt and fd" "crt 5 ask turtles [ fd 1 ]"
 "BEB43404EDC7852985A9A7FC312481785FE553A0")

(defsimplecommandtest "Wrapping 1" "crt 5 ask turtles [ fd 5 ]"
 "1098A56973DA04E7AEA7659C40E3FF3EC7862B02")

(defsimplecommandtest "Wrapping 2" "crt 5 ask turtles [ fd random-float 5 ]"
 "1419DFA66EFB7F08FB30C7B63B256547212EB915")

(defsimplecommandtest "Wrapping 3" "crt 10 ask turtles [ fd -5 ]"
 "53E4ECBD3C49FC8D3466563641CFCD7DCB5CD2AF")

(defsimplecommandtest "Wrapping 4" "crt 10 ask turtles [ fd random-float -5 ]"
 "1258CE9CC93B52367E797F4C497BF95760EC7175")

(defsimplereportertest "Random 1" "random-float 5" "4.244088516651127"
 "17D1BF7FF7BF2C7F3F5F7DD7CF67CFF2772CFFFC")

(defsimplereportertest "= 1" "5 = 5" "true"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "= 2" "5 = 4" "false"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "!= 1" "5 != 5" "false"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "!= 2" "5 != 4" "true"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "- 1" "5 - 5" "0"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "- 2" "5 - 6" "-1"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "- 3" "random-float 5 - random-float 5" "3.349608870016444"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "+ 1" "5 + 5" "10"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "+ 2" "5 + -6" "-1"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "+ 3" "random-float 6 + random-float 6" "6.166281795942972"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "* 1" "5 * 5" "25"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "* 2" "5 * -6" "-30"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "* 3" "random-float 4 * random-float 7" "4.251800892259665"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "/ 1" "5 / 5" "1"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "/ 2" "5 / -6" "-0.8333333333333334"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "/ 3" "random-float 4 / random-float 7" "2.7112896835726876"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "< 1" "5 < 5" "false"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "< 2" "5 < 6" "true"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "< 3" "random-float 4 < random-float 7" "false"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "<= 1" "5 <= 5" "true"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "<= 2" "5 <= 6" "true"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "<= 3" "random-float 4 <= random-float 7" "false"
 "811837B74F63D10ABBC01DD59C1E7556706D9F7A")

(defsimplereportertest "any? 1" "any? turtles" "false"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defreportertestwithsetup "any? 2" "crt 10" "any? turtles" "true"
 "A925E39EC022967568D238D31F70F0A375024A89")

(defsimplecommandtest "die 1" "crt 10 ask turtles [ die ]"
 "A665C1BF95E1F9CAAE9B9F8B2FBE3DAA45453136")

(defreportertestwithsetup "any? 3" "crt 10 ask turtles [ die ]" "any? turtles" "false"
 "A665C1BF95E1F9CAAE9B9F8B2FBE3DAA45453136")

(defsimplecommandtest "rt 1" "crt 100 ask turtles [ fd random-float 5 rt random-float 180 fd random-float 4 ]"
 "186B05DEFF6771BE791D54AB36A36874EC6E04FE")

(defsimplecommandtest "rt 2" "crt 100 ask turtles [ fd random-float 5 rt random-float 1080 fd random-float 4 ]"
 "154C05DF7810C0FF5D7DDE51B76E1012FFB2C0E1")

(defsimplecommandtest "lt 1" "crt 100 ask turtles [ fd random-float 5 lt random-float 180 fd random-float 4 ]"
 "D4B3844FE453C05E57537D6BA94C4B42C84655C6")

(defsimplecommandtest "lt 2" "crt 100 ask turtles [ fd random-float 5 lt random-float 1080 fd random-float 4 ]"
 "07DEB6F4F007DB86CD8F2C2E10BD4E35CAD2B0CE")

(defsimplecommandtest "if 1" "if 5 = 5 [ crt 10 ]"
 "A925E39EC022967568D238D31F70F0A375024A89")

(defsimplecommandtest "if 2" "if 5 = 4 [ crt 10 ]"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplecommandtest "ifelse 1" "ifelse 5 = 5 [ crt 10 ] [crt 5 ] if-else 5 = 5 [ crt 10 ] [ crt 5 ]"
 "2CF70DC9135754E77B64422C10E947E776E731E6")

(defsimplecommandtest "ifelse 2" "ifelse 5 = 4 [ crt 10 ] [ crt 5 ] if-else 5 = 4 [ crt 10 ] [ crt 5 ]"
 "A925E39EC022967568D238D31F70F0A375024A89")

(defsimplereportertest "colors 1" "green" "55"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplereportertest "colors 2" "black" "0"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defsimplecommandtest "let 1" "let a 5 crt a"
 "9FE588C2749CD9CE66CB0EA451EFB80476E881FB")

(defsimplecommandtest "let 2" "let a 5 let b 6 crt (a + b)"
 "4ABB6822402929878AB9E5A1084B9E4AE1F01D5B")

(defsimplecommandtest "ticks 1" "reset-ticks tick"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defreportertestwithsetup "ticks 1" "reset-ticks tick tick" "ticks" "2"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defreportertestwithsetup "of / who 1" "crt 10" "[ who ] of turtles" "[5 9 4 3 7 0 1 2 6 8]"
 "3F39BD2D8D5A1B2333E6C0DB665DBE3DCD5A75CE")

(defreportertestwithsetup "set / pcolor" "ask patches [ set pcolor green ]" "[ pcolor ] of patches"
 "[55 55 55 55 55 55 55 55 55]"
 "3E246C518581E004BC65EFB074A09BA2EEBB2910")

(defsimplereportertest "one-of 1" "one-of patches" "(patch -1 -1)"
 "0BDACB8E9D2BB768C01826E993B47D83D39FBD0C")

(defsimplereportertest "one-of 2" "one-of turtles" "nobody"
 "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")

(defreportertestwithsetup "one-of 3" "crt 10" "one-of turtles" "(turtle 5)"
 "A056ED8BF26A69FB4437E79F263E362C27F8820E")

(defsimplecommandtest "color 1" "crt 10 ask turtles [ set color green ]"
 "20943094E2C70D5A12AC6EEB29E8E9E2D21BD87D")

(defsimplecommandtest "label 1" "crt 10 ask turtles [ set label who ]"
 "96BF63544678A06E0D9A5062613CE1CAD638FCD5")

(defsimplecommandtest "label-color 1" "crt 10 ask turtles [ set label-color green ]"
 "70AB2BAA0BFD312256DDE6C02EE2B9C23E9B3532")

(defsimplecommandtest "size 1" "crt 10 ask turtles [ set size 5 ]"
 "8837CF2681A2091B0664FAA2C32062B19F548ED6")

(defsimplereportertest "random 1" "random 100000" "85402"
 "17D1BF7FF7BF2C7F3F5F7DD7CF67CFF2772CFFFC")

(defreportertestwithsetup "random 2" "crt 10" "[ random 1000000 ] of turtles"
 "[512564 490953 127774 976371 218233 692751 909837 655769 977588 485347]"
 "2048ED1C553B0342D5DE1302577394CD09DE88DA")

(defsimplecommandtest "setxy 1" "crt 10 ask turtles [ setxy random-xcor random-ycor ]"
 "B02FD5B864A129AED5254A68C499607F7F6EA236")
