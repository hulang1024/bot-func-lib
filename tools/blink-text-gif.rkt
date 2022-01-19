(define (text-gif text [sec 1])
  (define (image->bitmap image)
    (define bm (make-bitmap (image-width image) (image-height image)))
    (define dc (send bm make-dc))
    (send dc set-smoothing 'aligned)
    (render-image image dc 0 0)
    (send dc set-bitmap #f)
    bm)
  (define bitmaps null)
  (define delay-csec 10)
  (for ([n (/ (* (min sec 3) 100) delay-csec)])
    (define text-color (color (random-integer 0 255)
                              (random-integer 0 255)
                              (random-integer 0 255)))
    (define text-image (text/font text 24 "black" "NSimSun" 'modern 'normal 'bold #f))
    (define image (overlay text-image
                           (rectangle (+ (image-width text-image) 40)
                                      (+ (image-height text-image) 20)
                                      "solid"
                                      text-color)))
    (set! bitmaps (append bitmaps (cons (image->bitmap image) null))))
  (define filename (string-append (number->string (current-milliseconds) 16) "-text.gif"))
  (define path (build-path __data-dir-path filename))
  (write-animated-gif bitmaps delay-csec path
                      #:loop? #t
                      #:one-at-a-time? #t
                      #:disposal 'restore-bg)
  (__eval-output-add-image #:path (path->string path)))