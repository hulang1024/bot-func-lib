(define temp-dir "x")

(define (display-image image)
  (display-media "image" image))

(define (display-audio audio)
  (display-media "audio" audio))

(define (display-media type content)
  (cond
    [(bytes? content)
      (let ([filename (write-binary-file content)])
        (display (format "[s:~A:id=~A]" type filename)))]
    [(string? content) (display (format "[s:~A:url=~A]" type content))]))

(define (write-binary-file bytes)
  (define filename (real->int (current-inexact-milliseconds)))
  (define out (open-output-file (format "~A/~A" temp-dir filename) #:mode 'binary))
  (write-bytes bytes out)
  (close-output-port out)
  filename)
