#lang racket

(provide init-user-lib)

(define (init-user-lib env)
  (init-bot-func-lib env))

(define (init-bot-func-lib env)
  (define lib-dir "/home/eval-server/lib")
  (define (eval-file path env)
    (define in (open-input-file path #:mode 'text))
    (let loop ()
      (define expr (read in))
      (when (not (eof-object? expr))
        (eval expr env)
        (loop))))

  (displayln "lib loading")
  (for ((filename
         '("base"
           "media-output"
           "menu"
           "ascii-screen"

           "games/bulls-and-cows"
           "games/simple-guess-number"
           "games/tic-tac-toe"
           "games/tic-tac-toe-online"
           "games/cards/card"
           "games/cards/guess-card"

           "fz-rkt-lib"

           "tools/analog-clock"
           "tools/digital-clock"
           "tools/blink-text-gif"
           "tools/dict"
           "tools/joke"
           "tools/citys"
           "tools/query-weather"
           "tools/pic"
           "tools/sorry")))
    (displayln (string-append " " filename))
    (eval-file (format "~A/~A.rkt" lib-dir filename) env)))