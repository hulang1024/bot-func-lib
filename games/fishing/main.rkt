(define (make-fish name weight) (cons name weight))
(define (fish-name fish) (car fish))
(define (fish-weight fish) (cdr fish))

(define fish-basket%
  (class object%
    (init max-size user-id)
    (super-new)
    
    (define _user-id user-id)
    (define store (make-vector max-size #f))
    (define count 0)

    (define/public (get-user-id) _user-id)

    (define/public (add-fish fish)
      (when (not (full?))
        (vector-set! store count fish)
        (set! count (+ count 1))))

    (define/public (full?)
      (= count (vector-length store)))

    (define/public (clear)
      (set! count 0)
      (vector-fill! store #f))

    (define/public (stat is-print)
      (define (print-item fish count)
        (displayln (format " ~A x ~A条" (fish-name fish) count)))
      (define total-count 0)
      (define total-weight 0)
      (for-each (lambda (fishs)
                  (when is-print
                      (print-item (first fishs) (length fishs)))
                  (set! total-count (+ total-count (length fishs)))
                  (let ([weight 0])
                    (for-each (lambda (fish)
                                (set! weight (+ weight (fish-weight fish))))
                              fishs)
                    (set! total-weight (+ total-weight weight))))
                (group-by (lambda (x) (fish-name x))
                          (filter (lambda (x) x)
                                  (vector->list store))))
      (when is-print
        (displayln (format "\n共计~A条，~A公斤" total-count total-weight)))
      (cons total-count total-weight))))


(define fishing-game%
  (class object%
    (super-new)

    (define users (make-hash))
    (define user-fish-baskets (make-hash))

    (define (add-user uid user)
      (hash-set! users uid user))
    
    (define (create-fish-basket uid)
      (let ([fish-basket (new fish-basket% [user-id uid] [max-size 6])])
        (hash-set! user-fish-baskets uid fish-basket)
        fish-basket))

    (define/public (fishing)
      (define fish-count (length fishs))
      
      (define random-fish-kind (real->int (floor (* (random) fish-count))))
      (define random-fish-weight (max (floor (* (random) 30)) 1))
      (define fish (make-fish (list-ref fishs random-fish-kind)
                              random-fish-weight))

      (display (format "~A 摸到了一条 ~A 公斤的 ~A。"
                       (hash-ref __sender 'nickname)
                       (fish-weight fish)
                       (fish-name fish)))

      (let ([fish-basket
             (if (hash-has-key? user-fish-baskets __sender-id)
                 (hash-ref user-fish-baskets __sender-id)
                 (begin
                   (display "\n帮助命令：!摸鱼帮助")
                   (add-user __sender-id __sender)
                   (create-fish-basket __sender-id)))])
        (if (send fish-basket full?)
            (display "\n放入鱼护失败，原因：鱼护满了")
            (begin
              (send fish-basket add-fish fish)
              (when (send fish-basket full?)
                (display "\n你的鱼护满了"))))))
    
   (define/public (stat-fish-basket user)
     (define user-id (hash-ref user 'id))
     (if (hash-has-key? user-fish-baskets user-id)
         (let ([fish-basket (hash-ref user-fish-baskets user-id)])
           (displayln (format "~A 的鱼的统计" (hash-ref user 'nickname)))
           (void (send fish-basket stat #t)))
         (display "你未摸过鱼")))

    (define/public (clear-fish-basket)
     (if (hash-has-key? user-fish-baskets __sender-id)
         (let ([fish-basket (hash-ref user-fish-baskets __sender-id)])
           (send fish-basket clear)
           (display (format "~A的鱼已清空" (hash-ref __sender 'nickname))))
         (display "你未摸过鱼")))

    (define/public (ranking by top)
      (define (print-stats fish-basket-stats)
        (display "摸鱼排名")
        (display (cond
                   [(equal? by 'count) "按条数"]
                   [(equal? by 'weight) "按重量"]
                   [(equal? by 'overall) ""]))
        (displayln (format " TOP ~A\n" top))

        (define no 1)
        (for-each (lambda (item)
                    (let* ([fish-basket (car item)]
                           [user (hash-ref users (send fish-basket get-user-id))]
                           [stats (cdr item)]
                           [total-count (car stats)]
                           [total-weight (cdr stats)])
                      (when (<= no top)
                        (displayln (format "#~A ~A ~A条，~A公斤"
                                           (~a no
                                               #:align 'left
                                               #:width 2
                                               #:pad-string " ")
                                           (~a (hash-ref user 'nickname)
                                               #:align 'left
                                               #:width 12
                                               #:pad-string " ")
                                           total-count
                                           total-weight))))
                    (set! no (+ no 1)))
                  fish-basket-stats))

      (let ([fish-basket-stats
             (map (lambda (fish-basket)
                    (cons fish-basket (send fish-basket stat #f)))
                  (hash-values user-fish-baskets))])
        (print-stats
         (sort fish-basket-stats >
               #:key (lambda (item)
                       (let* ([stat (cdr item)]
                             [count (car stat)]
                             [weight (cdr stat)])
                         (cond
                           [(equal? by 'count) count]
                           [(equal? by 'weight) weight]
                           [(equal? by 'overall) count * weight])))))))))
         

(define fishing-game (new fishing-game%))
(define-name-command 摸鱼帮助
  (display "摸鱼命令：\n !摸鱼\n !我的鱼\n !清空我的鱼\n !摸鱼排名\n !摸鱼排名按条数\n !摸鱼排名按重量"))
(define-name-command 摸鱼 (send fishing-game fishing))
(define-name-command 钓鱼 摸鱼)
(define-name-command 我的鱼 (send fishing-game stat-fish-basket __sender))
(define-name-command 清空我的鱼 (send fishing-game clear-fish-basket))
(define-name-command 摸鱼排名 (send fishing-game ranking 'overall 50))
(define-name-command 摸鱼排名按条数 (send fishing-game ranking 'count 50))
(define-name-command 摸鱼排名按重量 (send fishing-game ranking 'weight 50))