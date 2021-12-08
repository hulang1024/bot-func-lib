;(require "../ascii-screen.rkt")

(define tic-tac-toe%
  (class object%
    (super-new)
    (define board (make-vector 3))
    (define screen (new ascii-screen% [width 5] [height 4]))

    (define (init-board)
      (for ((r (range 3)))
        (vector-set! board r (make-vector 3))))

    (init-board)

    (define (draw-board-no)
      (for ((y (range 4)) (p '("1" "2" "3" " ")))
        (send screen set 0 y p)
        (send screen set 1 y " " ))
      (for ((x (range 3)) (p '("1" "2" "3")))
        (send screen set (+ x 2) 3 p)))

    (draw-board-no)

    (define (board-set! r c chess)
      (vector-set! (vector-ref board r) c chess))
 
    (define (board-get r c)
      (vector-ref (vector-ref board r) c))
      
    (define (clear-board)
      (for ((r (range 3)))
        (for ((c (range 3)))
          (board-set! r c 0))))

    (define (update-screen)
      (for ((r (range 3)))
        (for ((c (range 3)))
          (let* ([chess (board-get r c)]
                 [p (if (= chess 0)
                        "+"
                        (if (= chess 1) "o" "x"))])
            (send screen set (+ c 2) r p)))))

    (define/public (start)
      (clear-board)
      (update-screen)
      (send screen draw))

    (define/public (put r c chess)
      (if (= (board-get r c) 0)
          (begin
            (board-set! r c chess)
            (update-screen)
            (send screen draw)
            #t)
          (begin
            (send screen draw)
            (display "无效落子：已有棋子")
            #f)))))
