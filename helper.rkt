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

(define driver
  (let ([build-file "build.s"])
    (lambda (debug-passes build-passes inputs)
      (for-each 
        (lambda (input)
          (printf "~a~n" input)
          (printf "~a~n" (evalify ((apply pipe debug-passes) input)))
          (with-output-to-file build-file #:exists 'replace
            (lambda () ((apply pipe build-passes) input)))
          (system (format "cc runtime.c ~a && ./a.out" build-file))
          (printf "~n"))
        inputs))))

(define evalify
  (lambda (program)
    (let ([ns (module->namespace 'racket)])
      (for-each 
        (lambda (expr) (eval expr ns))
       `((define rax (box 0)) (define rbx (box 0)) (define rcx (box 0)) (define rdx (box 0)) 
         (define rsi (box 0)) (define rdi (box 0)) (define rbp (box 0)) (define rsp (box 0))
         (define r8  (box 0)) (define r9  (box 0)) (define r10 (box 0)) (define r11 (box 0))
         (define r12 (box 0)) (define r13 (box 0)) (define r14 (box 0)) 
         (define r15 (box (lambda () (unbox rax))))
         (define stack (list->vector (map (lambda (x) (box 0)) (range 25))))
         (define-syntax locate (syntax-rules () [(_ bind* body ...) (let bind* body ...)]))
         (define (nop) (void)) (define (true) #t) (define (false) #t)
         (define (sra x n) (arithmetic-shift x (- n)))
         (define (logand x y) (bitwise-and x y))
         (define (logor x y) (bitwise-ior x y))))
      (eval program ns))))

; ----- helper ----- ;
(define pipe
  (lambda fs
    (lambda (arg)
      (foldl (lambda (f a) (f a)) arg fs))))
