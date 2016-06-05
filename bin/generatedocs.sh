#!/bin/bash

generatedoc() {
  package=$1
  sbcl \
    --eval "(asdf:load-system :docgen)" \
    --eval "(asdf:load-system :clnl)" \
    --eval "(format t \"----~%\")" \
    --eval "(format t \"~A\" (docgen:export-package $package))" \
    --eval "(quit)" 2> /dev/null | sed -n '/^----$/,$p' | tail -n +2
}

generatedoc :clnl > wiki/DocsMain.md
generatedoc :clnl-nvm > wiki/DocsNvm.md
generatedoc :clnl-interface > wiki/DocsOtherPackages.md
generatedoc :clnl-cli > wiki/DocsOtherPackages.md
generatedoc :clnl-lexer >> wiki/DocsOtherPackages.md
generatedoc :clnl-parser >> wiki/DocsOtherPackages.md
generatedoc :clnl-transpiler >> wiki/DocsOtherPackages.md
generatedoc :clnl-code-parser >> wiki/DocsOtherPackages.md
generatedoc :clnl-model >> wiki/DocsOtherPackages.md
generatedoc :clnl-random >> wiki/DocsOtherPackages.md
