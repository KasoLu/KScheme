#lang racket

(provide (all-defined-out))

; ----- syntax pred ----- ;
(define int?
  (lambda (i)
    (or (int32? i) (int64? i))))

(define int32?
  (lambda (x)
    (and (and (integer? x) (exact? x))
         (<= (- (expt 2 31)) x (- (expt 2 31) 1)))))

(define int64?
  (lambda (x)
    (and (and (integer? x) (exact? x))
         (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))

(define reg?
  (lambda (x)
    (memq x '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))))

(define var?
  reg?)

(define binop?
  (lambda (op)
    (memq op '(+ - *))))

; ----- func ----- ;
(define build #f)

(define debug #t)

(define test
  (lambda (pass #:catch [catch #f] . cases)
    (for-each 
      (lambda (c)
        (printf "~a~n" (pretty-format c))
        (if catch 
          (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
            (printf "~a~n" (pretty-format (pass c))))
          (printf "~a~n" (pretty-format (pass c))))
        (printf "~n"))
      cases)))
