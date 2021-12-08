#lang racket

(provide init-user-lib)

(define (init-user-lib env)
  (init-chat-bot-lib env))

(define (init-chat-bot-lib env)
  (define lib-dir "/Users/hulang/Documents/workspace/chat-bot-lib")
  (define (eval-file path env)
    (define in (open-input-file path #:mode 'text))
    ; 出现读一半的问题，加上begin 解决了
    (define expr-string (string-append "(begin " (port->string in) "\n)"))
    (define expr (read (open-input-string expr-string)))
    (eval expr env))

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
           "games/fishing/fishs"
           "games/fishing/main"
           "games/cards/card"
           "games/cards/guess-card"
            
           "tools/dict"
           "tools/joke"
           "tools/citys"
           "tools/query-weather"
           "tools/pic"
           "tools/sorry")))
    (displayln (string-append " " filename))
    (eval-file (format "~A/~A.rkt" lib-dir filename) env)))