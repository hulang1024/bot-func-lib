(define bulls-and-cows%
  (class object%
    (super-new)
    (define is-started #f)
    (define answer #f)
    (define answer-length 4)

    (define (generate-answer)
      (set! answer (make-vector answer-length))
      (let* ([seed-length 10]
             [seed-vec (list->vector (map (λ (n) (string-ref (number->string n) 0))
                                          (range seed-length)))])
        (for ([i (range answer-length)])
          (let ([r (random-integer 0 seed-length)])
            (vector-set! answer i (vector-ref seed-vec r))
            (vector-set! seed-vec r (vector-ref seed-vec (- seed-length i 1)))))))

    (define (feedback input)
      (let ([a 0] [b 0])
        (for ([i (range answer-length)])
          (if (char=? (vector-ref answer i) (string-ref input i))
              (set! a (+ a 1))
              (for ([j (range answer-length)])
                (when (and (not (= i j))
                         (char=? (vector-ref answer j) (string-ref input i)))
                    (set! b (+ b 1))))))
        (format "~AA~AB" a b)))

    (define/public (start)
      (generate-answer)
      (set! is-started #t)
      (displayln (format "😙猜数字开始了，已生成~A位的每位各不相同的一个数字。" answer-length))
      (display "使用 (我猜 x) 进行猜数，或发送 猜数字帮助"))

    (define/public (end)
      (set! is-started #f)
      (display "猜数字游戏结束。"))

    (define/public (show-answer)
      (when is-started
        (display (format "偷偷告诉你，答案是 ~A"
                         (list->string (vector->list answer))))))
  
    (define/public (guess input)
      (when (number? input)
        (set! input (number->string input)))
      (if is-started
          (if (= (string-length input) answer-length)
              (let ([output (feedback input)])
                (display (format "提示: ~A" output))
                (when (string=? output "4A0B")
                  (displayln "\n😀恭喜你，猜对了！\n")
                  (end)))
              (display (format "答案是~A位数" answer-length)))
          (start)))))


(define bulls-and-cows (new bulls-and-cows%))
(define-name-command 猜数字帮助
  (begin
    (displayln "提示的含义")
    (display "A 位置正确的数的个数\n")
    (display "B 数字正确但位置错误的数的个数")))
(define-name-command 猜数字 (send bulls-and-cows start))
(define-name-command 停止猜数字 (send bulls-and-cows end))
(define-name-command 猜数字答案 (send bulls-and-cows show-answer))
(define (我猜 x) (send bulls-and-cows guess x))
