;-- Faiz Racket Lib
(require (only-in rnrs/base-6 inexact))

;-- info
(define (ver) "2022-1-19")

;-- stx
(define-syntax def-syntax
  (syntax-rules ()
    ( [_ f (expr body ...)] ;must be bef (_ f g)
      (define-syntax f
        (syntax-rules ()
          (expr
            (begin body ...) ) ) ) ) ;
    ( [_ f g]
      (define-syntax f
        (syntax-rules ()
          ( [_ . args] ;
            (g . args) ) ) ) )
    ( [_ f (expr ...) ...]  ;
      (define-syntax f
        (syntax-rules ()
          (expr ...) ...
) ) ) ) )

(def-syntax defsyt def-syntax)

(define-syntax define*
  (syntax-rules ()
    ( [_ x] (define x [void]) ) ;
    ( [_ (g . args) body ...] ;
      (define (g . args) body ...) )
    ( [_ x e] (define x e) )
) )
(def-syntax def* define*)
;(def-syntax def define*)

;--

(def* *apis* ;tab?
  '(
    ;--
    ;--
    api-ls ver
    with? with-sym?
    ;--
    lisp tru? fal?
    key->val val->key
    flat rev foldl curry rcurry compose
    ;-- root
    car cdr cons cond quote eq atom
    ;-- stx
    if~ def/values def-syntax
    setq let def lam
) )

(define-syntax define*/doc
  (syntax-rules ()
    ( [_ x] (define x [void]) )
    ( [_ (g . args) body ...]
      (bgn ;
        (if (member 'g *apis*) (void) ;
          (set! *apis* (cons 'g *apis*)) )
        (define (g . args) body ...) ) )
    ( [_ x e] (define x e) )
) )
(defsyt def*/doc define*/doc)
(defsyt def/doc define*/doc)
(defsyt def define*/doc)

(defsyt defm ;define-macro <- define-syntax ;to-add (void)
  ( [_ (f . args) body ...]
    (defsyt f ;
      ( [_ . args]
        (begin body ...) ;
  ) ) )
  ( [_ f (args ...) body ...]
    (defm (f args ...) body ...)
) )
(defsyt alias defsyt) ;

;-- alias
(alias ali alias)
(alias bgn begin)

(ali   lam      lambda)
(ali   letn     let*)
(ali   fn       lambda)

(alias def-syt define-syntax)
(alias syt-ruls syntax-rules)
(alias syt-case syntax-case) ;
(alias case-lam case-lambda)

(alias imp      import) ;
(alias progn    begin)
(alias quo      quote)

;-- macro
(def-syt (if% stx)
  (syntax-case stx (else) ;
    ( [_ () (bd ...)]
      #'(cond bd ...) )
    ( [_ (last-expr) (bd ...)]
      #'(if% () (bd ... [else last-expr])) )
    ( [_ (k e more ...) (bd ...)]
      #'(if% (more ...) (bd ... [k e]))
) ) )
;
(def-syt (if~ stx)
  (syntax-case stx ()
    ([_ bd ...]
      #'(if% [bd ...] []) ;
) ) )

(defsyt defn/values-help
  ([_ f (p ... q) (V ... Q) ret ...]
    (defn/values-help f (p ...) (V ...)
      ret ... ([p ...] (f p ... Q))
  ) )
  ([_ f (p ...) () ret ...]
    (case-lam ret ...) ;@
) )
;
(defsyt def/values
  ([_ (f p ...) [V ...] body ...]
    (def f
      [defn/values-help f (p ...) [V ...] ;
        ([p ...] body ...) ]
) ) )

(define-syntax (push stx)
  (syntax-case stx () ;
    ( [_ args ... x]
      (identifier? #'x) ;
      #'(setq x (cons* args ... x)) ) ;;
    ( [_ . args]
      #'(cons* . args)
) ) )

;--- def
(def eq eq?)
;
(def char->int  char->integer)
(def int->char  integer->char)
(def sym->str symbol->string)
(def list->str  list->string)
(def str->sym   string->symbol)
(def str->list string->list)
(def str->chs   string->list)
(def exa->inexa exact->inexact)
(def inexa->exa inexact->exact)

(def *will-ret* #t)


;(def tail list-tail)
(defm (raw . xs) ;(str '|c:\asd\|)
  (if~
    (nilp 'xs) nil
    (nilp (cdr 'xs))
      (car 'xs)
    'xs
) )


(defsyt defs ;inner
  ((_ a) (def a (void)))
  ((_ a b)
    (bgn (def a b) (if *will-ret* a (void))) ;
  )
  ((_ a b c ...)
    (bgn
      (def a b)
      (defs c ...) ;
) ) )


(defsyt setq% ;
  ( [_ a b]
    (bgn ;(def a nil)
      (set! a b)
      (if *will-ret* a (void)) ;
  ) )
  ( [_ a]
    (setq% a (void)) )
  ( [_ a b c ...]
    (bgn
      (setq% a b)
      (setq% c ...) ;
) ) )
(alias setq setq%)
;(defsyt setq defs)

(defs T #t  F #f  *v (void)  nil '())

(def mem? member)
(def len length)

;(def *map/fn-lam* nil)

(def (head% xs m)
  (def (_ xs n)
    (cond
      ([nilp xs] nil)
      ([< n 1]   nil)
      (else (cons (car xs) [_ (cdr xs) (- n 1)]))
  ) )
  (_ xs m)
)

(def head head%) ;


;(def (unique-apis) (setq *apis* (remov-same *apis*)))

(defm (use-am% pre mid xs) (display pre) (display mid) (for-each (lam(x) (display x)(display " ")) xs)) ;
(defm (use-am . xs) (use-am% "am." "" 'xs)) ;
(defm (am% mid xs) (use-am% "am." mid xs))
(defm (am . xs) (am% "(load \"lib.sc\") " 'xs))


;---

(def (quiet . xs) (void))

(def (deep-length xs)
  (def (_ x n)
    (if~
      [nilp  x] n
      [consp x]
      (_ (car x)
        [_ (cdr x) n] )
      (+ n 1) ;
  ) )
  (_ xs 0)
)
(def deep-len deep-length)

(def (bump xs fmt) ;bumpl/bumpup-lhs
  (def (_ x fmt ret)
    (if~
      [nilp  fmt] ret
      [consp fmt]
        (letn ([fa (car fmt)] [fd (cdr fmt)] [ln (deep-len fa)] [head. (head x ln)] [tail. (tail x ln)])
          (if (consp (car fmt))
            (cons [_ head. fa ret] [_ tail. fd ret]) ;cons _ _
            [_ (car x) fa [_ tail. fd ret]] ;car
        ) )
      (cons x ret)
  ) )
  (_ xs fmt nil)
)

(def (display* . xs)
  (def (_ xs)
    (if (nilp xs) (void)
      (bgn
        (display (car xs)) (display " ") ;pretty-print
        [_ (cdr xs)]
  ) ) )
  (_ xs)
)

(def disp  display)

(def disp* display*)

;--- bef simple def

(def (atom? x) (not (pair? x)))

(define (append/rev-head xs ys) ;rev xs then append
  (define (_ ys xs)
    (if (null? xs) ys
      [_ (cons (car xs) ys) (cdr xs)]
  ) )
  (_ ys xs)
)

(define (cons* . xs)
  (define (~ ret xs)
    (let ([a (car xs)] [d (cdr xs)]) ;
      (if [null? d] (append/rev-head ret a) ;
        (~ (cons a ret) d) 
  ) ) )
  [~ nil xs]
)

(def (fold-left f x xs)
  (redu f (cons x xs)) ;
)
;---

(def   ev     eval)
(def   reduce apply) ;
(def   redu   apply) ;
(def   strcat string-append)


(def   foldl  fold-left) ;
;(def   foldr  fold-right)
(def   mod    remainder) ;
(def   %      mod) ;
(def   ceil   ceiling)

(def eql equal?) ;

(def sym?   symbol?)
(def bool?  boolean?)
(def int?   integer?)
(def num?   number?)
(def str?   string?)
(def consp  pair?)
(def pairp  pair?)
(def fn?    procedure?)
(def voidp  void?) ;voip
;(def atomp  atom?) ;?

(def nilp null?)
(def (id x) x)

;---

(def (pair->list% pr) ;'(1 . ()) ~> '(1 ())
  (list (car pr) (cdr pr))
)

(def (any->str x)
  (cond
    ((symbol? x) (sym->str            x)) ;
    ((bool?   x)  "") ;               x
    ((number? x) (number->string      x))
    ((char?   x) (list->string (list  x)))
    ((string? x)                      x)
    ((list?   x) (lis->str            x)) ;
    ((pair?   x) (lis->str(pair->list% x)))
    ;((ffi?   x) (sym->str           'x)) ;name?
    ((fn?     x)  "") ;ty?
    ((void?   x)  "")
    ((atom?   x) (sym->str           'x)) ;
    (else          "")
) )

(def (lis->str xs) ;
  (redu strcat (map any->str xs)) ;
) ;lis2str
 
(def my-range ;-> (0 ... n-1)
  (case-lambda
    ( [s e] ;~
      (def (_ ret e)
        (if (< e s) ret ;
          [_ (cons e ret) (- e 1)] ;
      ) )
      (_ nil e) )
    ( [n] (range n) ) ;
    ( [s e p] ;@
      (let ([g (if [>= p 0] > <)])
        (def (_ i)
          (if (g i e) nil
            (cons i [_ (+ i p)])
        ) )
        (cons s [_ (+ s p)]) ;
    ) )
    ( [s e f p] ;n ;0 1
      (let ([g (if (>= [f s p] s) > <)]) ;sign
        (def (_ i)
          (if (g i e) nil ;f ;range x m M ;min-max xs
            (cons i [_ (f i p)])
        ) )
        (cons s [_ (f s p)]) ;init
    ) )
    ( [total s f p e] ;
      (let ([g (if [>= (f s p) s] > <)]) ;
        (def (~ res x)
          (let ([res (- res x)])
            (if~ (or [< res 0] [g x e]) ;
              nil
              (cons x [~ res (f x p)]) ;cons
        ) ) )
        [~ total s]
) ) ) )

(def qsort
  (case-lam
    ([xs f] (sort xs f)) ;
    ([xs]   (qsort xs <))
) )

(def/values (tail% xs m tail?) [F]
  (def (_ xs m)
    (if~
      (nilp xs) nil
      (< m 1) xs
      [_ (cdr xs) (- m 1)]
  ) )
  (if tail?
    (_ xs (- (len xs) m))
    (_ xs m)
) )
(def tail tail%)

(def (curry g . args) ;
  (lam xs
    (apply g ;
      (append args xs) ;
) ) )

(def (rcurry g . args)
  (lam xs
    (apply g
      (append xs args)
) ) )

(def (nx->list n x) ;a bit faster than make-list
  (def (_ n rest)
    (cond
      [(< n 1) rest]
      [else
        (_ (- n 1) [cons x rest]) ] ; cons is fast
  ) )
  (_ n nil)
)

;---

(def/values (compress% x n xs =) [eq]
  (def (_ xs x n)
    (if (nilp xs)
      (list [list x n]) ;
      (let
        ([a(car xs)][d(cdr xs)])
        (if (= x a) ;
          (_ d x (+ n 1))
          (cons [list x n] (_ d a 1))
  ) ) ) )
  (_ xs x n)
)
(def/values (compress xs =) [eq] ;
  (if (nilp xs) nil
    (compress% (car xs) 1 (cdr xs) =)
) )
(def/values (decompress xz append?) [T] ;
  (let
    ( (tmp
        (map (lam (xs) (nx->list (cadr xs) (car xs))) xz)
    ) )
    (if append? (redu append tmp) tmp)
) )

(def/values (sort-by-sames xs g h append?) [> > T] ;>/</nil
  (letn
    ( [pre (if [nilp g] xs (qsort xs g))]
      [tmp (compress pre)]
      (next
        (if [nilp h] tmp
          (qsort tmp
            [lam (x y) (h (cadr x) (cadr y))]
    ) ) ) )
    (decompress next append?)
) )
(defm (api-with . xs) ;
  (let ~ ([ret (syms)] [ys 'xs]) ;
    (if (nilp ys) ret
      (let ([a(car ys)][d(cdr ys)])
        (~ [filter (rcurry with-sym? a) ret] d) ;
) ) ) )

;---

;(def (syms%) (environment-symbols (interaction-environment))) ;
(def (syms) *apis*)
(def (apis) *apis*)

;(exception? (try (cdr '())))
;(try (cdr '(1)))

;---


(def (rev xs)
  (def (_ ret xs)
    (if (nilp xs) ret
      (_ [cons (car xs) ret]
        (cdr xs)
  ) ) )
  [_ nil xs]
)

(def (flat xs)
  (def (_ ret x) ;
    (cond
      ([nilp x] ret)
      ([consp x]
        (_ [_ ret (cdr x)] (car x)) )
      (else (cons x ret))
  ) )
  (_ nil xs)
)

(define (fib0 n)
  (define (~ n)
    (case n
      ([0 1] n) ;
      (else
        (+ [~ (-  n 2)] ;
           [~ (-  n 1)]
  ) ) ) )
  (~ n)
)

(def (deep-reverse xs)
  (def (d-rev xs)
    (cond
      ([nilp xs] '()) ;
      ([pair? xs]
        (map d-rev (rev xs)) ) ;
      (else xs)
  ) )
  (d-rev xs)
)

(def with-head?
  (case-lam
    ( [xs ys eql] ;(_ '(1 2 3 4) '(1 2/3)) ;-> Y/N
      (def (_ xs ys)
        (cond
          [(nilp ys) T] ;
          [(nilp xs) F]
          ((eql (car xs) (car ys))
            [_ (cdr xs) (cdr ys)])
          [else F]
      ) )
      (_ xs ys) )
    ( [xs ys]
      (with-head? xs ys eql)
) ) )

(def with? ;may need algo
  (case-lam
    ( [xs ys eql] ;(_ '(1 2 3 4) '(2 3/4)) ;-> Y/N ;with/contain-p
      (def (_ xs)
        (if (nilp xs) F
          (if (with-head? xs ys eql) T ;output?
            [_ (cdr xs)] ;
      ) ) )
      (if (nilp ys) F
        (_ xs)
    ) )
    ( [xs ys]
      (with? xs ys eql)
) ) )

(def (with-sym? s x) ;sym/with?
  (redu (rcurry with? eq) ;
    (map (compose str->list sym->str) ;str-downcase
      (list s x) ;
) ) )


;===

(def randnums
  (case-lam
    ([n s e] ;e-s+1 + s ;range?
      (let ([m (- (1+ e) s)])
        (def (_ ret n)
          (if (< n 1) ret ;
            [_ (cons [+ s (random m)] ret) (1- n)] ;
        ) )
        (_ nil n)
    ) )
    ([n m]
      (def (_ ret n)
        (if (< n 1) ret
          [_ (cons (random m) ret) (1- n)] ;
      ) )
      (_ nil n)
    )
    ([n] [randnums n n])
) )

(def (count YS xs)
  (def (~ xs ys i)
    (if [nilp xs] i
      (let ([a(car xs)][d(cdr xs)])
        (if [nilp ys]
          (~ xs YS (1+ i))
          (let ([ay (car ys)] [dy (cdr ys)])
            (if [eq a ay]
              (~ d dy i)
              (~ d YS i)
  ) ) ) ) ) )
  (~ xs YS 0)
)

(def (remov-extras@ extras xs)
  ( (redu compose
      [map (lam (x) (curry remove x)) extras])
    xs
) )
(ali remov-extras remov-extras@)

(def (int n) ;[exa? F]
  (def (_ n)
    (if (number? n)
      (cond
        ([inexact? n] (inexact->exact (round n))) ;
        ([int? n]     n                         )
        ([_ (inexact n)]                        ) )
      n ;
  ) )
  (_ n)
)

;---

;(def rest cdr)

(def (deep-map g xs) ;deep-map g . xz
  (def (_ xs)
    (if [nilp xs] nil
      (let ([a(car xs)][d(cdr xs)])
        (if [consp a] ;
          (cons [_ a] [_ d]) ;flat & bump/hunch ;@
          (cons (g a) [_ d])
  ) ) ) )
  [_ xs]
)

;--- def/values

(def/values (key->val xz x = f-case defa? defa) [eql id F nil] ;
  (def (_ xz)
    (if (nilp xz)
      (if defa? defa x)
      (let ([xs (car xz)] [yz (cdr xz)])
        (if [= (f-case x) (car xs)] ;src-case
          [if (nilp [cdr xs]) nil (cadr xs)] ;
          [_ yz]
  ) ) ) )
  (_ xz)
)

(def/values (val->key xz x = f-case) [eql id] ;
  (def (_ xz)
    (if (nilp xz) x
      (let ([xs (car xz)] [yz (cdr xz)]) ;hlp _ ~
        (if (nilp (cdr xs)) x ;
          (if [= (f-case x) (cadr xs)] ;
            (car xs) ;
            [_ yz]
  ) ) ) ) )
  (_ xz)
)

(def/values (string-divide s sep) [" "]
  (letn
    ( [conv string->list]
      [deconv list->string]
      [xs (conv s)]
      [xsep (conv sep)] )
    (map deconv (divide xs xsep))
) )

;===
(def *will-disp* T)

(def (echo% sep . xs) ;(_ xs [sep " "]) ;voidp
  (def (_ sep xs)
    (case [len xs] ;
      ([0] (void))
      ([1] (disp (car xs))) ;
      (else
        (disp (car xs) sep)
        [_ sep (cdr xs)] ;
  ) ) )
  (if *will-disp*
    (_ sep xs)
    *v
) )

(def (echol . xs)
  (redu display* xs)
  (newline)
)

(def/values (avg% ns n0 f g) [0 + /] ;* pow/ 4 2
  (def (~ ns ret n)
    (if [nilp ns]
      (g ret n) ;
      (let
        ( [a(car ns)][d(cdr ns)] )
        (~ d (f a ret) (+ n 1)) ;
  ) ) )
  (~ ns n0 0) ;0
)
(def (.avg . xs)
  (avg% xs 0.)
)

(def (1+ x) (+ x 1))
(def (1- x) (- x 1))

(def (avg . xs) ;@
  (avg% xs)
)

(def inexa exact->inexact)

(def/values (pow r n) [2] (expt r n))

(def (round% x) ;
  (let ([fl (floor x)])
    (if [>= (- x fl) 0.5] ;
      (1+ fl)
      fl
) ) )

(def myround ;when input exa flt, is (4~)6x ~ than round
  (case-lambda
    ( [flt] ;(exact (#%round [inexa flt]))
      (round flt) ) ;when input inexa flt, is 1.1(~4.5)x @ than round
    ( [flt nFlt] ;if exa should ret int
      (let ([fac (pow 10. nFlt)]) ;
        (/ [round% (* fac [inexa flt])] fac) ;
) ) ) )

(def/values (in-range% num s e le) [<=]
  (if [and (le s num) (le num e)] T F)
)
(def inrange in-range%)
(def deep-rev deep-reverse)

(def (divide xs sep)
  (def (_ ret elem tmp xs ys) ;
    (if~
      (nilp ys)
        [_ (cons elem ret) nil nil xs sep]
      (nilp xs) ;atomp
        (cons (append tmp elem) ret) ;~append
      (let ([ax (car xs)] [dx (cdr xs)] [ay (car ys)] [dy (cdr ys)]) ;
        (if (eq ax ay) ;
          [_ ret elem (cons ax tmp) dx dy] ;
          [_ ret (cons ax (append tmp elem)) nil dx sep]
  ) ) ) )
  [deep-rev (_ nil nil nil xs sep)] ;
)
(def (divides xs seps)
  (def (_ xz seps)
    (if (nilp seps) xz
      (_ [redu append (map [rcurry divide (car seps)] xz)] ;
        (cdr seps)
  ) ) )
  [_ (list xs) seps]
)

(def (flow . xs) (redu compose (rev xs)))

(def (str . xs) (lis->str xs))

(defsyt type-main ;todo detail for printf
  ( [_ x]
    (cond
      ;((ffi-s? (any->str 'x))  "ffi") ;x if not sym
      ((symbol?  x)   "symbol") ;
      ((bool? x)      "boolean")
      ((number?  x)   "number") ;
      ((char? x)      "char") ;
      ((string?  x)   "string") ;
      ((nilp  x)      "null")
      ((list? x)      "list") ;
      ((pair? x)      "pair")
      ;((ffi?  x)     "ffi") ;
      ((fn?   x)      "fn") ;procedure
      ((vector? x)    "vector") ;
      ((void? x)      "void")
      ((atom? x)      "other-atom") ;(eof-object) ;x (void) #err
      (else           "other")  ;other-atoms
) ) )
(alias ty type-main)

(def (remov-nil xs)
  (def (_ xs)
    (if (nilp xs) nil
      (let ([a (car xs)] [d (cdr xs)])
        (if (nilp a)
          [_ d]
          (cons a [_ d]) ;
  ) ) ) )
  [_ xs]
)

(def (redu-map r m xs) (redu r (map m xs)))
(def (sym< x y) (redu-map string<? sym->str (list x y)))
(def (sym> x y) (redu-map string>? sym->str (list x y)))

(def (atom-cmp x y)
  (letn
    ( [type (ty x)]
      (<>
        (case type
          (["string"] (list string<? string>?))
          (["char"  ] (list char<? char>?))
          (["symbol"] [list sym< sym>]) ;
          (["number"] (list < >)) ;vector
          (else nil)
    ) ) )
    (if (eq type (ty y))
      (if (nilp <>)
        (if (eql x y) = nil) ;
        (cond
          ([(car  <>) x y] '<)
          ([(cadr <>) x y] '>)
          (else '=)
      ) )
      nil ;
) ) )

(def (neq x y) (not (eq x y)))
(def (ty-neq x y)
  (neq (ty x) (ty y))
)

(def (any-cmp xs ys)
  (def (_ xs ys)
    (if [nilp xs] '= ;
      (if [consp xs] ;? < atom nil pair list
        (let ([x (car xs)] [y (car ys)])
          (if [consp x]
            (let ((resl [_ x y]))
              (case resl
                ([=] [_ (cdr xs) (cdr ys)])
                (else resl)
            ) )
            (if [ty-neq x y] nil ;
              (let ([resl (atom-cmp x y)]) ;
                (case resl
                  ([=] [_ (cdr xs) (cdr ys)]) ;
                  (else resl)
        ) ) ) ) )
        (atom-cmp xs ys) ;
  ) ) )
  (_ xs ys)
)
(def (any> x y) [eq (any-cmp x y) '>])
(def (any< x y) [eq (any-cmp x y) '<])
(def (any= x y) [eq (any-cmp x y) '=])

(ali api-ls api-with)

(def #%in-range in-range)

;---

(def/values (festival x key->val?) [T]
  (let
    ( (kvs ;date-day
        '(
          [12-25 Christmas]
          [1-1 New-year]
          [2-1 Spring-festival]
          [3-8 Women-s-day]
          [4-1 April-fools-day]
          [5-1 May-day]
          [6-1 Children-s-day]
          [7-1 Army-day]
          [8-1 The-party-s-birthday]
          [9-10 Teachers-day]
          [10-1 National-day]
          [11-11 Singlers-day]
          [12-12 Double-twelve]
    ) ) )
    (if key->val?
      (key->val kvs x)
      (val->key kvs x)
) ) )

(defm (admin b)
  (if [eq 'b 'on]
    (bgn (setq *admin?* T) (disp "恭喜获得 GM 权限!"))
    (bgn (setq *admin?* F) (disp "无权限"))
) )

(def *admin?* F)
