((elm-mode
  (elm-interactive-command . ("elm" "repl"))
  (elm-reactor-command . ("elm" "reactor"))
  (elm-reactor-arguments . ("--port" "8000"))
  (elm-compile-command . ("elm" "make"))
  (elm-compile-arguments . ("--output=elm.js" "--debug"))
  (elm-package-command . ("elm" "package"))
  (elm-package-json . "elm.json")))
