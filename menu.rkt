(define (menu)
  (define func-texts
    '("菜单"
      "更多"
      "(美音 \"hi\")"
      "(英音 \"hi\")"
      "<位置>天气 ;或 (天气 <位置>)"
      "笑话"
      "动漫"
      "sorry"
      "猜牌"
      "猜数字"
      "简单猜数字"
      "井字棋"
      "摸鱼"))

  (displayln "☂菜单")
  (displayln "  ========")
  (for ((item func-texts))
    (displayln (format "  ~A" item)))
  (displayln "  ========"))

(define-name-command 菜单 (menu))
(define-name-command 帮助 (menu))
(define-name-command 更多
  (begin
    (displayln "发送前缀【#】或【!】加上代码以执行程序，支持Racket/Scheme语言，如 #(+ 1 2)")
    (displayln "前缀#的执行结果会带源代码引用，而!的执行结果不带源代码引用。")
    (display "（如果是一个完整的S表达式开头，如(+ 1 2)，则可以不带任何前缀。）")))