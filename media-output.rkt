(define temp-dir "x")

(define (display-image image)
  (display-media "image" image))

(define (display-audio audio)
  (display-media "audio" audio))

(define (display-media type content)
  (cond
    [(bytes? content)
     (define (write-binary-file bytes)
       (define filename (real->int (current-inexact-milliseconds)))
       (define out (open-output-file (format "~A/~A" temp-dir filename) #:mode 'binary))
       (write-bytes bytes out)
       (close-output-port out)
       filename)
     (let ([filename (write-binary-file content)])
       (display (format "[s:~A:id=~A]" type filename)))]
    [(string? content) (display (format "[s:~A:url=~A]" type content))]))


(require net/url
         'http-util)

(define (display-image/redirect url)
  (define-values (status headers in)
    (http-sendrecv/url (string->url url)))
  (cond
    [(string-contains? (bytes->string/utf-8 status) "302")
     (define location (get-header-value #"Location" headers))
     (when location
       (set! location (string-replace location " " "%20"))
       (display-image location))]
    [else (error (format "[~a]的请求未响应重定向,请尝试使用 display-image" url))]))


