(define (draw-clock-image hour minute second)
  ;by 雨 qq158675958
  (overlay 
   (circle 6  'solid  'gold)
   (rotate (* second -6)
           (above (rectangle 2 80 'solid 'gold)(make-bitmap 2 60 )))
   (rotate (+ (* minute -6) (* second -0.1))
           (above (rectangle 4 60 'solid 'blue)(make-bitmap 4 40 )))
   (rotate (+ (* (/ (remainder  hour 12) -12) 360) (* minute -0.5))
           (above (rectangle 8 45 'solid 'red)(make-bitmap 4 25 )))
   (apply overlay (append (map (lambda(x)(rotate (* x 30)
                                                (above (rectangle 4 20 'solid 'black)(make-bitmap 4 180 ))))(range 12))
                          (map (lambda(x)(rotate (* x 6)
                                                 (above (rectangle 2 10 'solid 'blue)(make-bitmap 4 190 ))))(range 60))))
   (above (text "λ" 30 'black)
          (make-bitmap 4 100 ))
   (circle 100  'outline  'gold)))


(define (clock-gif)
  (define bitmaps null)
  (define duration-s 30)
  (define (image->bitmap image)
    (define bm (make-bitmap (image-width image) (image-height image)))
    (define dc (send bm make-dc))
    (send dc set-smoothing 'aligned)
    (render-image image dc 0 0)
    (send dc set-bitmap #f)
    bm)
  
  (for ([s duration-s])
    (define now (seconds->date (+ (current-seconds) s)))
    (define hour (date-hour now))
    (define minute (date-minute now))
    (define second (date-second now))
    (define image (draw-clock-image hour minute second))
    (set! bitmaps (append bitmaps (cons (image->bitmap image) null))))
  (define filename (string-append (number->string (current-milliseconds) 16) "-clock.gif"))
  (define path (build-path __data-dir-path filename))
  (write-animated-gif bitmaps 100 path
                      #:loop? #f
                      #:one-at-a-time? #t
                      #:disposal 'restore-bg)
  (__eval-output-add-image #:path (path->string path)))


(define-name-command 钟 (clock-gif))
  