(struct card (flower point))

(define (print-card card)
  ((lambda ()
     (define graph-table '("♠" "♥" "♦" "♣"))
     (display (string-append
               (list-ref graph-table (- (card-flower card) 1))
               (card-point card))))))

(define card-pack%
  (class object%
    (init init-cards)
    (super-new)

    (define cards init-cards)

    (define/public (get-cards) cards)
    
    (define/public (cards-count)
      (length cards))

    (define/public (print)
      (for ([card cards])
        (print-card card)
        (display "  "))
      (newline))

    (define/public (shuffle-cards)
      (set! cards (shuffle cards)))

    (define/public (deal n)
      (let ([sub-cards (make-vector n (list))])
        (for ([i (range (length cards))])
          (let* ([card (list-ref cards i)]
                 [j (modulo i n)]
                 [sub (vector-ref sub-cards j)])
            (vector-set! sub-cards j (append sub (list card)))))
        sub-cards))))
