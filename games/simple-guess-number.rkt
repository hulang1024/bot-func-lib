(define simple-guess-number%
  (class object%
    (super-new)
    (define is-started #f)
    (define answer 0)
    (define max 100)

    (define (match-answer n)
      (display (if (= n answer)
                   (begin
                     (displayln (string-append "😀 " (hash-ref __sender 'nickname) " 答对了!"))
                     (end))
                   (if (> n answer) "😅大了" "😂小了"))))

    (define/public (start)
      (set! answer (floor (* (random) max)))
      (set! is-started #t)
      (display (string-append "😙猜数字开始了，已生成"
                              (number->string max)
                              "内的数，使用 (我猜 x) 进行猜数。")))

    (define/public (end)
      (set! is-started #f)
      (display "猜数字游戏结束"))
  
    (define/public (guess n)
      (if is-started
          (match-answer n)
          (start)))))


(define simple-guess-number (new simple-guess-number%))
(define-name-command 简单猜数字 (send simple-guess-number start))
(define-name-command 停止简单猜数字 (send simple-guess-number end))
(define (简单猜数字我猜是 x) (send simple-guess-number guess x))
