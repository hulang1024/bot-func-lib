;(require "./card.rkt")

(define guess-card%
  (class object%
    (super-new)

    (define status_init 0)
    (define status_created 1)
    (define status_started 2)

    (define status status_init)
    (define total-step 4)
    (define step 0)
    (define pack #f)
    (define sub-packs (list))
    (define participant #f)

    (define (init-pack)
      (define cards (list))
      (let ([i 0])
        (for ([point '("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K")])
          (for ([flower (range 4)])
            (set! cards (append cards (list (card (+ flower 1) point))))
            (set! i (+ i 1)))))
      (set! pack (new card-pack% [init-cards cards])))

    (define/public (create)
      (init-pack)
      (displayln (format "你好，~A，这是一个猜牌魔术。" (hash-ref __sender 'nickname)))
      (displayln (format "首先请从以下~A张牌中随意【记住一张牌】。\n" (send pack cards-count)))
      (set! status status_created)
      (send pack print)
      (display "\n如果已经记住，现在开始请不要更换。\n确认请发送 猜牌开始"))

    (define/public (end)
      (set! status status_init)
      (set! step 0)
      (set! participant #f))

    (define/public (start)
      (cond
        [(= status status_init) (create)]
        [(= status status_created)
         (set! participant __sender)
         (set! status status_started)
         (send pack shuffle-cards)
         (displayln "接下来你需要回答你心中的牌出现在下面的哪个序列中。\n发送 猜牌选择x，x写1，2或3 ")
         (next-step)]
        [else (display "已经开始")]))

    (define (next-step)
      (when (< 0 step (- total-step 1))
        (displayln (format "还剩~A次哦" (- total-step step 1))))
      (when (> step 1)
        (displayln (if (> (random) 0.5) "嗯...需要再确认下" "我好像知道了，不过需要再确认下")))
      (set! step (+ step 1))
      (set! sub-packs (list))
      (for ([sub-cards (send pack deal 3)])
        (let ([sub-pack (new card-pack% [init-cards sub-cards])])
          (set! sub-packs (append sub-packs (list sub-pack)))
          (newline)
          (send sub-pack print))))
    
    (define/public (choose n)
      (cond
        [(= status status_init) (display "请先创建")]
        [(= status status_created) (display "请先开始")]
        [(and (= status status_started) (< 0 step (+ total-step 1)))
         (if (equal? (hash-ref __sender 'id) (hash-ref participant 'id))
             (if (<= 1 n 3)
                 (let* ([satck-indexs (cond
                                        [(= n 1) '(2 1 3)]
                                        [(= n 2) '(1 2 3)]
                                        [(= n 3) '(1 3 2)])]
                        [stack-cards (flatten (map (lambda (i)
                                                     (send (list-ref sub-packs (- i 1)) get-cards))
                                                   satck-indexs))])
                   (set! pack (new card-pack% [init-cards stack-cards]))
                   (if (>= step total-step)
                       (begin
                         (print-answer)
                         (end))
                       (next-step)))
                 (display "无效的选择，请选择1、2、3"))
             (display (format "~A 已在进行中" (hash-ref participant 'nickname))))]))

    (define/public (print-answer)
      (define cards (send pack get-cards))
      (define cards-count (send pack cards-count))
      (define answer-card (list-ref cards (floor (/ cards-count 2))))
      (display (format "~A，你的牌是" (hash-ref participant 'nickname)))
      (print-card answer-card))))

(define guess-card (new guess-card%))
(define-name-command 猜牌 (send guess-card create))
(define-name-command 猜牌开始 (send guess-card start))
(define-name-command 猜牌结束 (send guess-card end))
(define-name-command 猜牌选择1 (send guess-card choose 1))
(define-name-command 猜牌选择2 (send guess-card choose 2))
(define-name-command 猜牌选择3 (send guess-card choose 3))