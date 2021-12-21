(module osu-api racket
  ; https://github.com/ppy/osu-api/wiki
  (require net/url
           net/uri-codec
           json)

  (#%provide (rename get-user osu-api/get-user))

  (define (get-user #:u u #:mode [mode 0])
    (get-json 'get_user `((u . ,u) (m . ,mode))))
        
  
  (define base-url "osu.ppy.sh/api")
  
  (define api-key "8d81085d1374ea124c124283fe7612b7cb35dbd8")
  
  (define (get-json path params)
    (define url (string->url
                 (format "https://~A/~A?k=~A&~A"
                         base-url
                         (symbol->string path)
                         api-key
                         (alist->form-urlencoded (map (λ (p)
                                                        (let ([k (car p)] [v (cdr p)])
                                                          (cons k (cond
                                                                    [(number? v) (number->string v)]
                                                                    [else v]))))
                                                      params)))))
    (define-values (status headers in)
      (http-sendrecv/url url))
    (string->jsexpr (port->string in))))

 
(require 'osu-api)

(define osu-user-manager%
  (class object%
    (super-new)

    (define osu-user-ids (make-hash))
    
    (define/public (get-users) osu-user-ids)
    
    (define/public (bind-user uid osu-uid)
      (hash-set! osu-user-ids uid osu-uid))

    (define/public (get-osu-user-id uid)
      (hash-ref osu-user-ids uid))

    (define/public (has-binding? uid)
      (hash-has-key? osu-user-ids uid))
    
    (define mode-name-hash
      (hash 0 "osu!"
            1 "Taiko"
            2 "CatchTheBeat"
            3 "osu!mania"))

    (define display-result-key-zh-hash
      (hash 'username "用户名"
            'country "国家"
            'pp_rank "PP排名"
            'pp_country_rank "PP国家排名"
            'pp_raw "PP"
            'accuracy "Acc"
            'playcount "游玩次数"
            'ranked_score "已排名分数"
            'total_score "总分数"
            'level "当前等级"
            'count300 "300x"
            'count100 "100x"
            'count50 "50x"
            'count_rank_ss "SS"
            'count_rank_s "S"
            'count_rank_a "A"
            'mode "模式"))
    
    (define display-orderedy-keys
      '(
        username
        country
        pp_rank pp_country_rank pp_raw
        accuracy
        playcount
        ranked_score
        total_score
        level
        count300 count100 count50
        count_rank_ss count_rank_s count_rank_a))

    (define/public (query-osu-user osu-uid mode)
      (define result (osu-api/get-user #:u osu-uid #:mode mode))
      (if (not (null? result))
          (let ([result (list-ref result 0)])
            (displayln (string-append "模式：" (hash-ref mode-name-hash mode)))
            (for ([key display-orderedy-keys])
              (when (and (hash-has-key? result key)
                         (hash-has-key? display-result-key-zh-hash key))
                (let ([title (hash-ref display-result-key-zh-hash key)]
                      [text (hash-ref result key)])
                  (displayln (string-append
                              title
                              (build-string (- 14 (text-width title)) (λ (n) #\space))
                              text))))))
          #f))))
          

(define osu-user-manager (new osu-user-manager%))

(define (osu-stat-user mode)
  (λ (osu-uid)
    (let ([found (send osu-user-manager query-osu-user osu-uid mode)])
      (when (not found)
        (printf "未找到osu用户 ~a\n" osu-uid)))))

(define-name-command osu-stat (osu-stat-sender 0))
(define-name-command taiko-stat (osu-stat-sender 1))
(define-name-command ctb-stat (osu-stat-sender 2))
(define-name-command mania-stat (osu-stat-sender 3))

(define (osu-stat-sender mode)
  (if (send osu-user-manager has-binding? __sender-id)
      ((osu-stat-user mode) (send osu-user-manager get-osu-user-id __sender-id))
      (display "请先发送:\n(设置osu用户 你的osu用户名或id)\n\n如：(设置osu用户 \"problue\")")))

(define osu-stat-u (osu-stat-user 0))
(define taiko-stat-u (osu-stat-user 1))
(define ctb-stat-u (osu-stat-user 2))
(define mania-stat-u (osu-stat-user 3))

(define (设置osu用户 osu-uid)
  (send osu-user-manager bind-user __sender-id osu-uid)
  (displayln "设置成功\n")
  osu)

(define-name-command osu
  (begin
    (displayln "osu命令菜单\n========")
    (let ([modes '(osu taiko ctb mania)])
      (displayln "自己的分数统计")
      (displayln "\t(设置osu用户 你的osu用户名或id)")
      (for ([mode modes])
        (displayln (format "\t!~A-stat" (symbol->string mode))))
      (displayln "别人的分数统计")
      (for ([mode modes])
        (displayln (format "\t(~A-stat-u osu用户名或id)" (symbol->string mode)))))))