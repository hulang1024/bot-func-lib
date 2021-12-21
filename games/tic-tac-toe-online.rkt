;(require "../ascii-screen.rkt")

(define tic-tac-toe-online%
  (class object%
    (super-new)
    
    ; 游戏待创建
    (define status_init 1)
    ; 游戏准备中(未满足游戏开始条件)
    (define status_ready 2)
    ; 游戏进行中
    (define status_playing 3)
    ; 游戏结束
    (define status_ended 4)

    (define status status_init)
    (define creator-uid 0)
    (define creator #f)
    (define other #f)
    (define chess-to-user (make-hash))
    (define user-to-chess (make-hash))
    (define now-chess 0)
    (define tic-tac-toe (new tic-tac-toe%))

    (define (swap-chess)
      (define (print-user user)
        (displayln (format "~A ~A ~A"
                         (let* ([active-user (hash-ref chess-to-user now-chess)]
                                [active (equal? user active-user)])
                           (if active ">" " "))
                         (if (= (hash-ref user-to-chess (hash-ref user 'id)) 1) "o" "x")
                         (hash-ref user 'nickname))))
      (set! now-chess (if (= now-chess 0)
                          1
                          (if (= now-chess 1) 2 1)))
      (print-user creator)
      (print-user other))

    (define (start)
      (displayln "井字棋游戏开始! 落子命令：(井字棋落子 横坐标 纵坐标) ")
      (set! status status_playing)
      (send tic-tac-toe start)
      (hash-set! chess-to-user 1 creator)
      (hash-set! user-to-chess (hash-ref creator 'id) 1)
      (hash-set! chess-to-user 2 other)
      (hash-set! user-to-chess (hash-ref other 'id) 2)
      (swap-chess))
    
    (define/public (create)
      (cond
        [(or (= status status_init)
             (= status status_ended))
         (set! creator-uid __sender-id)
         (set! creator (hash-copy __sender))
         (set! other #f)
         (set! status status_ready)
         (display "井字棋游戏创建成功！\n另一个玩家要加入游戏请发送：加入井字棋")]
        [(= status status_ready)
         (if (= __sender-id creator-uid)
             (display "请等待其它玩家加入")
             (join))]
        [(= status status_playing) (display "井字棋游戏进行中")]))

    (define/public (join)
      (cond
        [(or (= status status_init)
             (= status status_ended))
         (create)]
        [(= status status_ready)
         (if (= __sender-id creator-uid)
             (display "请等待其它玩家加入")
             (begin
               (set! other (hash-copy __sender))
               (start)))]
        [(= status status_playing) (display "井字棋游戏进行中")]))

    (define/public (end)
      (if (= __sender-id creator-uid)
          (begin
            (set! status status_ended)
            (display "井字棋游戏结束"))
          (display "只有房主有权限结束游戏")))

    (define/public (put x y)
      (cond
        [(or (= status status_init)
             (= status status_ended))
         (display "游戏未创建，创建游戏请发送：井字棋")]
        [(= status status_ready)
         (if (= __sender-id creator-uid)
             (display "请等待其它玩家加入")
             (display "请先加入游戏，发送：加入井字棋"))]
        [(= status status_playing)
         (let ([is-arg-ok #t]
               [expected-user (hash-ref chess-to-user now-chess)])
           (if (= __sender-id (hash-ref expected-user 'id))
               (begin
                 (when (not (and (number? x) (<= 1 x 3)))
                   (set! is-arg-ok #f)
                   (display "参数错误"))
                 (when (not (and (number? y) (<= 1 y 3)))
                   (set! is-arg-ok #f)
                   (display "参数错误"))
                 (when is-arg-ok
                   (when (send tic-tac-toe put (- y 1) (- x 1) now-chess)
                     (swap-chess))))
               (display (format "\n此回合应轮到 ~A 落子" (hash-ref expected-user 'nickname)))))]))))

(define tic-tac-toe-online (new tic-tac-toe-online%))
(define-name-command 井字棋 (send tic-tac-toe-online create))
(define-name-command 加入井字棋 (send tic-tac-toe-online join))
(define-name-command 开始井字棋 (send tic-tac-toe-online start))
(define-name-command 结束井字棋 (send tic-tac-toe-online end))
(define (井字棋落子 x y) (send tic-tac-toe-online put x y))
