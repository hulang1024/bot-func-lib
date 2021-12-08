(define (语音 text)
  (dict-voice 0 text))

(define (美音 text)
  (dict-voice 0 text))

(define (英音 text)
  (dict-voice 1 text))

(define (dict-voice type text)
  ; type 0=美音，1=英音
  (set! text (string-replace text " " "%20"))
  (display-audio (format "http://dict.youdao.com/dictvoice?type=~A&audio=~A" type text)))
