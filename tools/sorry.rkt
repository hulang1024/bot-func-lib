(require net/url)
(require json)

(define sorry-template-hash
  #hash(
        (sorry . "sorry")
        (王境泽 . "wangjingze")
        (金坷垃 . "jinkela")
        (土拨鼠 . "marmot")
        (土拔鼠 . "marmot")
        (打工 . "dagong")
        (电动车 . "diandongche")))

(define (make-sorry-gif template-key subtitles)
  (if (hash-has-key? sorry-template-hash template-key)
      (let ([template (hash-ref sorry-template-hash template-key)])
        (define-values (status headers in)
          (http-sendrecv/url (string->url (format "https://sorry.xuty.tk/api/~A/make" template))
                             #:method #"POST"
                             #:data (jsexpr->string subtitles)))
        (define gif-path (port->string in))
        (display-image (format "https://sorry.xuty.tk~A" gif-path)))
      (begin
        (displayln (format "不支持的模板参数：~A" template-key))
        (sorry模板))))

(define (sorry-subtitles . text-list)
  (let ([h (make-hash)]
        [pos 0])
    (for ([text (if (list? (car text-list)) (car text-list) text-list)])
      (hash-set! h (string->symbol (number->string pos)) text)
      (set! pos (+ pos 1)))
    h))

(define-name-command sorry
  (begin
    (displayln "制作为所欲为gif（更多信息前往 https://github.com/xtyxtyx/sorry)。\n命令：")
    (displayln (string-append "(make-sorry-gif 模板 (sorry-subtitles 字幕文本列表))"
                            "  ;例如：(make-sorry-gif '土拨鼠 (sorry-subtitles \"啊\"))\n"))
    (displayln "sorry模板")))

(define (sorry模板)
  (displayln "支持的sorry模板：")
  (for ([key (hash-keys sorry-template-hash)])
    (displayln (string-append "  " (symbol->string key)))))