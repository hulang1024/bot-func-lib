(require racket/contract
         racket/class
         racket/unit
         racket/dict
         racket/include
         racket/pretty
         racket/math
         racket/match
         racket/shared
         racket/set
         racket/list
         racket/vector
         racket/string
         racket/bytes
         racket/function
         racket/path
         racket/place
         racket/future
         racket/port
         racket/promise
         racket/bool
         racket/stream
         racket/sequence
         racket/local
         racket/format
         racket/date
         (only-in picturing-programs/private/map-image real->int)
         (for-syntax racket/base))

(define-syntax-rule (define-name-command name body)
  (define-syntax name
    (lambda (stx)
      (syntax-case stx ()
        [name (identifier? (syntax name)) (syntax body)]))))
