#lang racket
(provide (all-defined-out))
(define (check-true b)
  (match b
    [#t #t]
    [#f #f]))
(define (check-false b)
  (match b
    [#f #t]
    [#t #f]))
;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [''()  #t]
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [(? symbol? x) #t]
    [`(box? ,x) (expr? x)]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    [`(let () ,e1)
       (expr? e1)]
    [`(let ((,v ,e0) . ,rest) ,e1)
       (and (valid-binding v) (expr? `(let (,@rest) ,e1)) (expr? e1))]
    [`(let* () ,e1)
       (expr? e1)]
    [`(let* ((,v ,e0) . ,rest ) ,e1)
       (and (valid-binding v) (expr? `(let* (,@rest) ,e1)) (expr? e1))]
    [`(string? ,x) (expr? x)]
    [`(string-ref ,string ,natural) (and (expr? string)
                                         (expr? natural))]
    [`(string-length ,string) (expr? string)]
    [`(make-string ,natural ,char) (and (expr? natural)
                                         (expr? char))]

    [_ #f]))

;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? e)
 (match e
    [''()  #t]
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [`(if ,x ,y ,z)
     (and (closed? x)
          (closed? y)
          (closed? z))]
    [`(,(? prim1?) ,x) (closed? x)]
    [`(,(? prim2?) ,x ,y) (and (closed? x) (closed? y))]
    [(list 'cond `(else ,z)) (closed? z)]
   
    [`(cond (,xs ,ys) . ,rest)
       (and (closed? xs)
            (closed? ys)
            (closed? `(cond ,@ rest )))]
    [`(let () ,e1) (closed? e1)]
    [`(let ((,x ,e0) . ,rest) ,e1)
         (and (closed? e0)
              (closed-helper? (list) (list x) `(let (,@rest) ,e1)))]
    [`(let* () ,e1) (closed? e1)]
    [`(let* ((,x ,e0) . ,rest ) ,e1)
         (and (closed? e0)
              (closed-helper? (list) (list x) `(let* (,@rest) ,e1)))]
    [_ #f]))

;; list expr -> boolean
(define (closed-helper? l0 l1 x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [`(if ,x ,y ,z)
     (and (closed-helper? l1 l1 x)
          (closed-helper? l1 l1 y)
          (closed-helper? l1 l1 z))]
    [`(,(? prim1?) ,x) (closed-helper? l1 l1 x)]
    [`(,(? prim2?) ,x ,y) (and (closed-helper? l1 l1 x)
                               (closed-helper? l1 l1 y))]
    [(? symbol? x) (is-member? x l1)]
    [`(cond (else ,z)) (closed-helper? l1 l1 z)]
    [`(cond (,xs ,ys) . ,rest)
     (and (closed-helper? l1 l1 xs)
          (closed-helper? l1 l1 ys)
          (closed-helper? l1 l1 `(cond ,@rest)))]
    [`(let () ,e1) (closed-helper? l1 l1 e1)]
    [`(let ((,x ,e0) . ,rest) ,e1)
     (let ((new x))
       (and (closed-helper? l0 l0 e0)
            (closed-helper? l0 (append l1 (list new)) `(let (,@rest) ,e1))))]
    [`(let* () ,e1) (closed-helper? l1 l1 e1)] 
    [`(let* ((,x ,e0) . ,rest) ,e1)
     (let* ((new x))
       (and (closed-helper? l0 l0 e0)
            (closed-helper? (append l1 (list new)) (append l1 (list new)) `(let* (,@rest) ,e1))))]
    [_ #f]))

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr length box unbox string-length string?
                      char? integer? boolean? zero? box? empty? cons?
                      ))))
(define (valid-binding x)
  (and (symbol? x)
       (not (memq x '(add1 sub1 abs - integer->char char->integer + -
                      car cdr length box unbox string? string-ref string-length make-string
                      char? integer? boolean? zero? box? empty? cons? = < <= char=? boolean=?
                      )))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+ cons - = < <= char=? boolean=? string-ref make-string))))

;;

(define (is-member? x l)
  (match l
    ['() #f]
    [(cons h t) (if (eq? x h) #t (is-member? x t))]))



