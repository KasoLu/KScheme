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

(define int32~64?
  (lambda (x)
    (and (int? x) (not (int32? x)))))

(define reg?
  (lambda (x)
    (memq x '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))))

(define loc?
  (lambda (x)
    (or (reg? x) (fvar? x))))

(define var?
  (lambda (x)
    (or (uvar? x) (loc? x))))

(define triv?
  (lambda (x)
    (or (var? x) (int? x) (label? x))))

(define binop?
  (lambda (op)
    (memq op '(+ - * logand logor sra))))

(define relop?
  (lambda (op)
    (memq op '(= > < >= <=))))

(define label?
  (lambda (x)
    (any->bool (label-match x))))

(define fvar?
  (lambda (x)
    (or (any->bool (fvar-match x)) (disp-opnd? x))))

(define uvar?
  (lambda (x)
    (any->bool (uvar-match x))))

(struct disp-opnd (reg offset) #:prefab)

; ----- helper ----- ;
(define label-match
  (lambda (x)
    (rx-match #rx"^.+?\\$(0|[1-9][0-9]*)$" x)))

(define fvar-match
  (lambda (x)
    (rx-match #rx"^fv(0|[1-9][0-9]*)$" x)))

(define uvar-match
  (lambda (x)
    (rx-match #rx"^.+?\\.(0|[1-9][0-9]*)$" x)))

(define label->index
  (lambda (x)
    (x->index label-match x)))

(define fvar->index
  (lambda (x)
    (x->index fvar-match x)))

(define uvar->index
  (lambda (x)
    (x->index uvar-match x)))

; ----- func ----- ;
(define test
  (lambda (pass #:trace [trace #f] . cases)
    (for-each 
      (lambda (c)
        (printf "~a~n" (pretty-format c))
        (if trace
          (printf "~a~n" (pretty-format (pass c)))
          (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
            (printf "~a~n" (pretty-format (pass c)))))
        (printf "~n"))
      (begin cases))))

(define driver
  (let ([build-file "build.s"])
    (lambda (debug-passes build-passes #:trace [trace #f] inputs)
      (define (run input)
        (printf "~a~n" (evalify ((apply pipe debug-passes) input)))
        (with-output-to-file build-file #:exists 'replace
          (lambda () ((apply pipe build-passes) input)))
        (system (format "cc runtime.c ~a && ./a.out" build-file)))
      (for-each 
        (lambda (input)
          (printf "~a~n" input)
          (if trace
            (run input)
            (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
              (run input)))
          (printf "~n"))
        (begin inputs)))))

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
         (define (logor x y) (bitwise-ior x y))
         (define (int64-truncate x)
           (if (= 1 (bitwise-bit-field x 63 64))
             (- (bitwise-bit-field (bitwise-not (sub1 x)) 0 64))
             (bitwise-bit-field x 0 64)))))
      (eval program ns))))

; ----- struct ----- ;
(define hash-env:make
  (lambda ()
    (cons (make-hash) '())))

(define hash-env:search
  (lambda (env var)
    (let loop ([env env])
      (match env
        [(? null?)
         (begin #f)]
        [(cons table prev)
         (hash-ref table var (lambda () (loop prev)))]))))

(define hash-env:extend
  (lambda (env [pairs '()])
    (cons (make-hash pairs) env)))

(define hash-env:modify!
  (lambda (env var val)
    (match env
      [(? null?)
       (begin #f)]
      [(cons table prev)
       (hash-set! table var val)])))

(define list-env:make
  (lambda () '()))

(define list-env:search
  (lambda (env var)
    (let loop ([env env])
      (match env
        [(? null?)
         (begin #f)]
        [(cons (cons e-var (box b-val)) prev)
         (if (equal? var e-var)
           (begin b-val)
           (loop prev))]))))

(define list-env:extend
  (lambda (env [pairs '()])
    (let loop ([pairs pairs])
      (match pairs
        [(? null?)
         (begin env)]
        [(cons (cons var val) prev)
         (cons (cons var (box val)) (loop prev))]))))

(define list-env:modify!
  (lambda (env var val)
    (let loop ([env env])
      (match env
        [(? null?)
         (begin #f)]
        [(cons (cons e-var e-val) prev)
         (if (equal? var e-var)
           (set-box! e-val val)
           (loop prev))]))))

(define list-env:map
  (lambda (env #:reverse [rev #f] func)
    (let loop ([env env] [res* '()])
      (match env
        [(? null?)
         (if (not rev)
           (reverse res*)
           (begin res*))]
        [(cons (cons e-var (box e-val)) prev)
         (loop prev (cons (func e-var e-val) res*))]))))

(define list-env:fold
  (lambda (env func)
    (let loop ([env env] [res* '()])
      (match env
        [(? null?)
         (begin res*)]
        [(cons (cons e-var (box e-val)) prev)
         (loop prev (func e-var e-val res*))]))))

; ----- utils ----- ;
(define pipe
  (lambda fs
    (lambda (arg)
      (foldl (lambda (f a) (f a)) arg fs))))

(define any->bool
  (lambda (x)
    (if (not x) #f #t)))

(define rx-match
  (lambda (rx x)
    (cond
      [(symbol? x)
       (regexp-match rx (symbol->string x))]
      [else
       (begin #f)])))

(define x->index
  (lambda (x-m x)
    (match (x-m x)
      [`(,_ ,idx)
        (string->number idx)]
      [else
        (begin #f)])))

(define sra
  (lambda (x n) (arithmetic-shift x (- n))))

(define all?
  (lambda ps
    (lambda (x)
      (if (null? ps)
        (begin #t)
        (and ((car ps) x) ((apply all? (cdr ps)) x))))))

(define any?
  (lambda ps
    (lambda (x)
      (if (null? ps)
        (begin #f)
        (or ((car ps) x) ((apply any? (cdr ps)) x))))))

(define symbol-format
  (lambda (fmt . vals)
    (string->symbol (apply format fmt vals))))
