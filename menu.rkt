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
      "井字棋"))

  (displayln "☂菜单")
  (displayln "  ========")
  (for ((item func-texts))
    (displayln (format "  ~A" item)))
  (displayln "  ========"))

(define-name-command 菜单 (menu))
(define-name-command 帮助 (menu))