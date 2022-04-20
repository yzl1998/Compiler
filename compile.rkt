#lang racket
(provide (all-defined-out))

;; An immediate is anything ending in #b0000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift))) ;;0b111
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)

(define imm-shift        (+ 2 result-shift))                    ;;5
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift))) ;;0b11111
(define imm-type-int     (arithmetic-shift #b00 result-shift))  ;;0b00 000
(define imm-type-bool    (arithmetic-shift #b01 result-shift))  ;;0b01 000
(define imm-type-char    (arithmetic-shift #b10 result-shift))  ;;0b10 000
(define imm-type-empty   (arithmetic-shift #b11 result-shift))  ;;0b11 000

(define imm-val-false    imm-type-bool) ;;8 0b0001000
(define imm-val-true     (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool)) ;;72 0b1001000

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type Imm = Integer | Boolean | Char | '()

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp)
    (call error)
    ret))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? imm? i)            (compile-imm i)]
    [(? symbol? x)         (compile-var x c)]
    [`(box ,e0)            (compile-box e0 c)]
    [`(unbox ,e0)          (compile-unbox e0 c)]
    [`(cons ,e0 ,e1)       (compile-cons e0 e1 c)]
    [`(car ,e0)            (compile-car e0 c)]
    [`(cdr ,e0)            (compile-cdr e0 c)]
    [`(add1 ,e0)           (compile-add1 e0 c)]
    [`(sub1 ,e0)           (compile-sub1 e0 c)]
    [`(zero? ,e0)          (compile-zero? e0 c)]
    [`(if ,e0 ,e1 ,e2)     (compile-if e0 e1 e2 c)]
    [`(+ ,e0 ,e1)          (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1)          (compile-- e0 e1 c)]
    ;; Assignment 5
    [(? string? s)         (compile-string s c)]
    [`(string? ,e0)        (compile-string? e0 c)]
    [`(string-ref ,e0 ,e1) (compile-string-ref e0 e1 c)]
    [`(string-length ,e0)  (compile-string-length e0 c)]
    [`(make-string ,e0 ,e1)  (compile-make-string e0 e1 c)]
    [`(box? ,e0)           (compile-box? e0 c)]
    [`(empty? ,e0)         (compile-empty? e0 c)]
    [`(cons? ,e0)          (compile-cons? e0 c)]
    [`(= ,e0 ,e1)          (compile-= e0 e1 c)]
    [`(< ,e0 ,e1)          (compile-< e0 e1 c)]
    [`(<= ,e0 ,e1)         (compile-<= e0 e1 c)]
    [`(char=? ,e0 ,e1)     (compile-char=? e0 e1 c)]
    [`(boolean=? ,e0 ,e1)  (compile-boolean=? e0 e1 c)]
    ;; From Assignment 4
    [`(abs ,e0)
        (let ((c0 (compile-e e0 c)))
            `(,@c0
              ,@assert-integer
              (mov rbx rax)
              (neg rax)
              (cmovl rax rbx)))]
    [`(- ,e0)
        (let ((c0 (compile-e e0 c)))
            `(,@c0
              ,@assert-integer
               (neg rax)))]
    [`(cond [else ,e0])
        (let ((c0 (compile-e e0 c)))
        `(,@c0))]
    [`(cond [,e-p0 ,e-a0] . ,rest)
        (let ((c-p0 (compile-e e-p0 c))
              (c-a0 (compile-e e-a0 c))
              (c2 (compile-e `(cond ,@rest) c))
              (l0 (gensym))
              (l1 (gensym)))
            `(,@c-p0
              (cmp rax 8) ; compare to #f
              (je ,l0)        ; jump to c2 if #f
              ,@c-a0
              (jmp ,l1)       ; jump past c2
              ,l0
              ,@c2
              ,l1))]
    
    [`(integer->char ,e0)
        (let ((c0 (compile-e e0 c))
              (l0 (gensym)))
          `(,@c0
            ,@assert-integer
            (mov rbx rax)
            (cmp rbx -32)      ; integer <= -1 (0 - 1) jump to error
            (jle err)
            (cmp rbx 35651584) ; integer >= 1114112(#x10FFFF + 1) jump to error
            (jge err)
            (cmp rbx 1835008)  ; integer >= 57344 jump to end
            (jge ,l0)
            (cmp rbx 1769472)  ; integer >= 55296 jump to err
            (jge err)
            ,l0
            (add rax ,imm-type-char)))]
    [`(char->integer ,e0)
        (let ((c0 (compile-e e0 c))
              (l0 (gensym))
              (l1 (gensym)))
           `(,@c0
            (mov rbx rax)
            (and rbx ,(type-pred->mask 'char?))
            (cmp rbx ,(type-pred->tag 'char?))
            (jne err)
            (add rax ,(- 0 imm-type-char))))]
    [`(integer? ,e0)
        (let ((c0 (compile-e e0 c))
              (l0 (gensym))
              (l1 (gensym)))
          `(,@c0
            (mov rbx rax)
            (and rbx ,(type-pred->mask 'integer?))
            (cmp rbx ,(type-pred->tag 'integer?))
            (jne ,l0)
            (mov rax 72)
            (jmp ,l1)
            ,l0
            (mov rax 8)
            ,l1))]
    [`(boolean? ,e0)
        (let ((c0 (compile-e e0 c))
              (l0 (gensym))
              (l1 (gensym)))
          `(,@c0
            (mov rbx rax)
            (and rbx ,(type-pred->mask 'boolean?))
            (cmp rbx ,(type-pred->tag 'boolean?))
            (jne ,l0)
            (mov rax 72)
            (jmp ,l1)
            ,l0
            (mov rax 8)
            ,l1))]
    [`(char? ,e0)
        (let ((c0 (compile-e e0 c))
              (l0 (gensym))
              (l1 (gensym)))
          `(,@c0
            (mov rbx rax)
            (and rbx ,(type-pred->mask 'char?))
            (cmp rbx ,(type-pred->tag 'char?))
            (jne ,l0)
            (mov rax 72)
            (jmp ,l1)
            ,l0
            (mov rax 8)
            ,l1))]
    
    ;; Continued Assignment 4
    [`(let () ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0))]

    [`(let ((,x ,e0) . ,rest) ,e1)
     (let ((c0 (bind-variables 0 `((,x ,e0) . ,rest) c))
           (c1 (compile-e e1 (update-cenv `((,x ,e0) . ,rest) c))))
       `(,@c0
         ,@c1))]

    [`(let* () ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0))]

    [`(let* ((,x ,e0) . ,rest) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e `(let* (,@rest) ,e1) (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]))

;; Assignment 5

(define (insert-string length s c)
  (match length
    [0 `()]
    [(? integer?)  (let ((current-char (string-ref s 0))
              (update-length (- length 1))
              (update-string (substring s 1)))
          (let ((c0 (compile-e current-char c))
                (c1 (insert-string update-length  update-string c)))
            `(,@c0
              ,@assert-char
              (mov (offset rdi 0) rax)
              (add rdi 8)
              ,@c1)))]
    [_      `((push rbp)
              (call error))]))

(define (compile-string s c)
  (let ((length (compile-e (string-length s) c))
        )
          `(,@length
            ,@assert-integer
            (mov (offset rdi 0) rax)  ;; natural number to heap
            (mov rcx rdi)
            (or rcx ,type-string)
            (add rdi 8)               ;; increment heap pointer
            ,@(insert-string (string-length s) s c)
            (mov rax rcx))))

(define (insert-make-string length char c)
  (match length
    [0 `()]
    [(? integer? x)
        (let ((c0 (compile-e char c))
              (c1 (insert-make-string (- length 1) char c)))
             `(,@c0
              ,@assert-char
              (mov (offset rdi 0) rax)
              (add rdi 8)
              ,@c1))]
    [_        `((push rbp)
                (call error))]))

(define (compile-make-string e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
          `(,@c0
            ,@assert-integer
            (mov (offset rdi 0) rax)  ;; natural number to heap
            (mov rbx rdi)
            (or rbx ,type-string)
            (mov (offset rsp ,(- (add1 (length c)))) rbx)
            (add rdi 8)               ;; increment heap pointer
            ,@c1
            ,@assert-char
            ,@(insert-make-string e0 e1 c)
            (mov rax (offset rsp ,(- (add1 (length c))))))))

(define (compile-string? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
          `(,@c0
            (mov rbx rax)
            (and rbx ,(type-pred->mask 'string?))
            (cmp rbx ,(type-pred->tag 'string?))
            (jne ,l0)
            (mov rax 72)
            (jmp ,l1)
            ,l0
            (mov rax 8)
            ,l1)))

(define (compile-string-ref e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym)))
          `(,@c0
            ,@assert-string
            (xor rax ,type-string)
            (mov (offset rsp ,(- (add1 (length c)))) rax) ;;??
            ,@c1
            ,@assert-integer
            (cmp rax -32)            ; integer <= -1 (0 - 1) jump to error
            (jle err)
            (mov rbx (offset rsp ,(- (add1 (length c)))))
            (cmp rax (offset rbx 0)) ; integer >= length
            (jge err)
            ,l0
            (sub rax 32)
            (add rbx 8)
            (cmp rax 0)
            (jge ,l0)
            (mov rax (offset rbx 0)))))

(define (compile-string-length e0 c)
  (let ((c0 (compile-e e0 c)))
          `(,@c0
            ,@assert-string
            (xor rax ,type-string) ;;untag string
            (mov rbx rax) 
            (mov rax (offset rbx 0)))))

(define (compile-box? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          (mov rbx rax)
          (and rbx ,(type-pred->mask 'box?))
          (cmp rbx ,(type-pred->tag 'box?))
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-empty? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          (mov rbx rax)
          (and rbx ,(type-pred->mask 'empty?))
          (cmp rbx ,(type-pred->tag 'empty?))
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-cons? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          (mov rbx rax)
          (and rbx ,(type-pred->mask 'cons?))
          (cmp rbx ,(type-pred->tag 'cons?))
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-= e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          ,@assert-integer
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1
          ,@assert-integer
          (mov rbx (offset rsp ,(- (add1 (length c)))))
          (cmp rax rbx)
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-< e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          ,@assert-integer
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1
          ,@assert-integer
          (mov rbx (offset rsp ,(- (add1 (length c)))))
          (cmp rax rbx)
          (jle ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-<= e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          ,@assert-integer
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1
          ,@assert-integer
          (mov rbx (offset rsp ,(- (add1 (length c)))))
          (cmp rax rbx)
          (jl ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-char=? e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          ,@assert-char
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1
          ,@assert-char
          (mov rbx (offset rsp ,(- (add1 (length c)))))
          (cmp rax rbx)
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

(define (compile-boolean=? e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (l0 (gensym))
        (l1 (gensym)))
        `(,@c0
          ,@assert-boolean
          (mov (offset rsp ,(- (add1 (length c)))) rax)
          ,@c1
          ,@assert-boolean
          (mov rbx (offset rsp ,(- (add1 (length c)))))
          (cmp rax rbx)
          (jne ,l0)
          (mov rax 72)
          (jmp ,l1)
          ,l0
          (mov rax 8)
          ,l1)))

;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

;; Any -> Boolean
(define (type-pred? x)
  (memq x '(integer?
            char?
            empty?
            boolean?
            box?
            cons?)))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

;; Imm -> Integer
(define (imm->bits i)
  (match i
    [(? integer? i) (arithmetic-shift i imm-shift)]
    [(? char? c)    (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(? boolean? b) (if b imm-val-true imm-val-false)]
    [''()           imm-type-empty]))


;; Variable CEnv -> Asm
(define (compile-var x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; Expr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; Expr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 0) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 1) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; Expr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 1)))))

;; Expr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 0)))))

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    ;; FIXME this should really do the type check *after* both
    ;; expression have executed
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

;; Expr Expr CEnv -> Asm
(define (compile-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    ;; FIXME this should really do the type check *after* both
    ;; expression have executed
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (mov rbx (offset rsp ,(- (add1 (length c)))))
      (neg rbx)
      (add rax rbx))))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

;; From Assignment 4
(define (bind-variables n l c)
  (match l
    ['() `()]
    [`((,x ,e0) . ,rest)
     (let ((c0 (compile-e e0 c))
           (c1 (bind-variables (+ n 1) `(,@rest) c)))
        `(,@c0
         (mov (offset rsp ,(- (add1 (+ (length c) n)))) rax)
         ,@c1)
       )]))

(define (update-cenv l c)
  (match l
    ['() c]
    [`((,x ,e0) . ,rest) (update-cenv `(,@rest) (cons x c))]))


(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))
;;
(define (type-pred->mask p)
  (match p
     [(or 'box? 'cons? 'string? 'pair?) result-type-mask]
     [_ imm-type-mask]))
 ;;
(define (type-pred->tag p)
  (match p
    ['box?     type-box]
    ['cons?    type-pair]
    ['string?  type-string]
    ['integer? imm-type-int]
    ['empty?   imm-type-empty]
    ['char?    imm-type-char]
    ['boolean? imm-type-bool]
    ['pair?    type-pair]))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'pair?))
(define assert-char    (assert-type 'char?))
(define assert-string  (assert-type 'string?))
(define assert-boolean (assert-type 'boolean?))
