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
