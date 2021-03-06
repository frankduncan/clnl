(in-package #:clnl-test)

(defviewtest "Nothing" ""
 "62B8B468D5ED63CDFB567C984E0CAB53DBD03CEB")

(defviewtest "Basic 1" "crt 1"
 "BB7774BC721E16BD92B18228BBBAC8D7BAAA6271")

(defviewtest "Basic 2" "crt 10 ask turtles [ fd 1 ]"
 '("D7A3F7FC99CB46A9AC07A5FFAC8DBE4F3C8DFAEE" "CDDDB68DC28E1D1EE72AE3C3474E91381E45D7AB"))

(defviewtest "Wrapping" "crt 10 ask turtles [ fd 6 ]"
 '("51DACC1A8EE0758F94E8C3C1EC46467D46F796D0" "E08B45180949AB58E3F75A07DDC3CC07BC71DFDB"))

(defviewtest "Die" "crt 10 ask turtles [ fd 1 ] ask turtles [ die ]"
 "62B8B468D5ED63CDFB567C984E0CAB53DBD03CEB")

(defviewtest "rt" "crt 20 ask turtles [ fd 2 rt 100 fd 2 ]"
 '("1C325D14717E92D6368EF3D0276250A49AC94E3C" "6B8AE7C1F8AAB44934EFC17D3F8DC02EA93D42D0"))

(defviewtest "lt" "crt 20 ask turtles [ fd 2 lt 100 fd 2 ]"
 '("5A9976BA3BFF49B9232CC8285E40709B43BB97C6" "24F764D346E607CD10C1CDA83CEF0091FDFBC280"))

(defviewtest "pcolor green" "ask patches [ set pcolor green ]"
 "90F5F4870955B9FF02224F00E3C9814B8A6F766E")

(defviewtest "size" "crt 10 ask turtles [ fd 2 set size 3 ] "
 '("E71BD61118B3B735DE4ADD2EF7897465084DD372" "6A4D9F29F10EAFCF5AB6156CCB35680EF4E41677"))

(defviewtest "sheep" "set-default-shape turtles \"sheep\" crt 10 ask turtles [ fd 2 set size 3 ] "
 '("6D86C178B84836F064C0084E9A0BDE3BACCA28A2" "33DD3FA4103731FA6A2EA675104CEEFCE16ADF54"))

(defviewtest "wolves" "set-default-shape turtles \"wolf\" crt 10 ask turtles [ fd 2 set size 3 ] "
 '("D455A70DBAD3195F23328B58B4D123934FEA0DC0" "4C108D1B2ED37A9C2152BE816E2B8947861333DE"))
