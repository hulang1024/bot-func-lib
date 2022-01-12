(define (display-image url)
  (__eval-output-add-image #:url url))


(define (display-audio url)
  (__eval-output-add-audio #:url url))


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


