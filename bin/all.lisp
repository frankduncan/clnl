(when (not (find-package :clnl)) (asdf:load-system :clnl))
(when (not (find-package :clnl-test)) (asdf:load-system :clnl-test))

(format t "~%~c[1;33mRunning Tests~c[0m~%" #\Esc #\Esc)
(when (not (clnl-test:run-all-tests))
 (format t "~c[1;31mFailed tests!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))

(when (not (find-package :syntax-checker)) (asdf:load-system :style-checker))
(format t "~%~c[1;33mChecking Style~c[0m~%" #\Esc #\Esc)
(when (not (syntax-checker:pretty-print-check-directory "src"))
 (format t "~c[1;31mFailed style check!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))
(format t "~c[1;32m- Style Passed!~c[0m~%" #\Esc #\Esc)

(when (not (find-package :docgen)) (asdf:load-system :docgen))
(format t "~%~c[1;33mChecking Docs~c[0m~%" #\Esc #\Esc)
(when (not (docgen:pretty-print-validate-packages
            :clnl :clnl-parser :clnl-random :clnl-transpiler :clnl-nvm :clnl-lexer :clnl-interface :clnl-cli :clnl-model))
 (format t "~c[1;31mFailed doc check!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))
(format t "~c[1;32m- Doc Check Passed!~c[0m~%" #\Esc #\Esc)

(format t "~c[1;30m--------------~c[0m~%" #\Esc #\Esc)
(format t "~c[1;32mBuild Success!~c[0m~%" #\Esc #\Esc)
(sb-ext:exit :code 0)
