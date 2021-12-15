(define ascii-screen%
  (class object%
    (init width height)

    (super-new)

    (define w width)
    (define h height)
    (define vec (make-vector height))
    (for ((i (range h)))
      (vector-set! vec i (make-vector w)))

    (define/public (set x y p)
      (vector-set! (vector-ref vec y) x p))
    
    (define/public (fill p)
      (for ((y (range h)))
        (for ((x (range w)))
          (set x y p))))

    (fill "+")
    
    (define/public (draw)
      (for ([r (range h)])
        (for ([c (range w)])
          (display (vector-ref (vector-ref vec r) c)))
        (displayln "")))))


(define (text-width text)
  (define (ch-width ch)
    (if (> (char->integer ch) 127) 2 1))
  (apply + (map (λ (ch) (ch-width ch)) (string->list text))))