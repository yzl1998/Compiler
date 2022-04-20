#lang racket
(require "interp.rkt"
         rackunit)

(define (read-prog p)
  (regexp-match "^#lang racket" p)
  (read p))

;; Code for submission needs to be in ".." directory
(require (only-in "../compile.rkt" compile)
         (only-in "../asm/interp.rkt" asm-interp)
         (only-in "../parse.rkt" parse)
         (only-in "../syntax.rkt" expr? closed?)
         (only-in "../lex.rkt" lex-string lex-port))


(check-equal?
 (parse (lex-string "#lang racket (let ((x 1)) x)"))
 '(let ((x 1)) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax tests

(check-true (expr? 7))
(check-true (expr? "asdf"))
(check-true (expr? ""))
(check-true (expr? #t))
(check-true (expr? #t))
(check-true (expr? #\a))
(check-true (expr? '(add1 #f)))
(check-true (expr? '(sub1 #f)))
(check-true (expr? '(abs #f)))
(check-true (expr? '(- #f)))
(check-true (expr? '(zero? #f)))
(check-true (expr? '(integer->char #f)))
(check-true (expr? '(char->integer #f)))
(check-true (expr? '(char? #f)))
(check-true (expr? '(integer? #f)))
(check-true (expr? '(boolean? #f)))
(check-true (expr? '(box "adsf")))
(check-true (expr? '(+ 1 2)))
(check-true (expr? '(- 1)))
(check-true (expr? '(- 1 2)))
(check-true (expr? 'x))
(check-true (expr? '(let () x)))
(check-true (expr? '(let ((x 1)) x)))
(check-true (expr? '(let ((x 1) (y 2)) x)))
(check-true (expr? '(let ((x 1) (y 2) (z 3)) x)))
(check-true (expr? '(string-length "asdf")))
(check-true (expr? '(string-ref "asdf" 0)))
(check-true (expr? '(= #f #f)))
(check-true (expr? '(< #f #f)))
(check-true (expr? '(string? #f)))
(check-true (expr? '(box? #f)))
(check-true (expr? '(empty? #f)))
(check-true (expr? '(cons? #f)))
(check-true (expr? '(unbox #f)))
(check-true (expr? '(car #f)))
(check-true (expr? '(cdr #f)))
(check-true (expr? '(make-string #f #f)))
(check-true (expr? '(= #f #f)))
(check-true (expr? '(< #f #f)))
(check-true (expr? '(<= #f #f)))
(check-true (expr? '(char=? #f #f)))
(check-true (expr? '(boolean=? #f #f)))
(check-true (expr? '(+ #f #f)))
(check-true (expr? '(- #f #f)))

(check-false (expr? '(let 1)))
(check-false (expr? '(let x 1)))
(check-false (expr? '(let x y 1)))
(check-false (expr? '(let (x y) 1)))
(check-false (expr? '(let ((x)) 1)))
(check-false (expr? '(let ((1 2)) 1)))
(check-false (expr? '(let ((abs 1)) 1)))
(check-false (expr? '(let ((string-ref 1)) 1)))
(check-false (expr? '(let ((+ 1)) 1)))
(check-false (expr? '(let ((string? 1)) 1)))
(check-false (expr? '(1)))
(check-false (expr? '(box)))
(check-false (expr? '(string-ref "asdf")))
(check-false (expr? '(+ 1 2 3)))
(check-false (expr? '(make-string #f)))
(check-false (expr? '(make-string #f #f #f)))

(check-true (closed? 7))
(check-true (closed? "asdf"))
(check-true (closed? ""))
(check-true (closed? #t))
(check-true (closed? #f))
(check-true (closed? #\a))
(check-true (closed? '(box "adsf")))
(check-true (closed? '(+ 1 2)))
(check-true (closed? '(- 1)))
(check-true (closed? '(- 1 2)))
(check-true (closed? '(let ((x 1)) x)))
(check-true (closed? '(let ((x 1) (y 2)) x)))
(check-true (closed? '(let ((x 1) (y 2) (z 3)) x)))
(check-true (closed? '(string-length "asdf")))
(check-true (closed? '(string-ref "asdf" 0)))
(check-true (closed? '(let ((x 1) (y 2))
                        (let ((z y))
                          (+ x z)))))

(check-false (closed? 'x))
(check-false (closed? '(let () x)))
(check-false (closed? '(let ((x 1)) y)))
(check-false (closed? '(let ((x 1) (y x)) y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler tests

(check-equal?
 (asm-interp (compile '(let ((x 1)) x)))
 (interp '(let ((x 1)) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random tests

(define tokens
  (parameterize ((current-directory "progs"))
    (for/list ([fn (directory-list)])
      (call-with-input-file fn lex-port))))

(define parses
  (parameterize ((current-directory "progs"))
    (for/list ([fn (directory-list)])
      (call-with-input-file fn read-prog))))

(for ([t tokens]
      [p parses])
  (check-not-exn (lambda () (parse t)) t)
  (check-equal? (parse t) p))

(for ([p parses])
  (check-true (and (expr? p)
                   (closed? p))))

(for ([p parses])
  (check-equal? (asm-interp (compile p))
                (interp p)
                p))
