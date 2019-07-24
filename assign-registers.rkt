#lang racket

(require racket/trace)
(require "helper.rkt")
(provide assign-registers)

(define assign-registers
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (let
          ([map-lmd (lambda (l b) `(,l (lambda () ,b)))]
           [map-b* (map $body b*)])
         `(letrec ,(map map-lmd l* map-b*) ,($body b)))])))

(define $body
  (lambda (body)
    (match body
      [`(locals (,u* ...) (register-rconflict ,cg ,t))
        (hook
          ([prev (lambda () (u/r-map:init!))]
           [post (lambda () (u/r-map:reset!))])
          (u/r-cfl:analyze! cg)
         `(locate ,(u/r-map:->list) ,t))])))

; ----- helper ----- ;
(define u/r-map (void))

(define u/r-map:init!
  (lambda ()
    (set! u/r-map (make-hash))))

(define u/r-map:reset!
  (lambda ()
    (set! u/r-map (void))))

(define u/r-map:->list
  (lambda ()
    (hash->list u/r-map)))

(define u/r-map:search
  (lambda (uvar)
    (hash-ref u/r-map uvar #f)))

(define u/r-map:update!
  (lambda (uvar reg)
    (hash-set! u/r-map uvar reg)))

(define u/r-cfl:analyze!
  (lambda (ori-cg)
    (let loop ([cg ori-cg])
      (match cg
        [(? null?)
         (hash->list u/r-map)]
        [(cons (cons uvar u/r-cfl) next)
         (u/r-cfl->reg! uvar u/r-cfl ori-cg)
         (loop next)]))))

(define u/r-cfl->reg!
  (lambda (uvar u/r-cfl cg)
    (match (u/r-map:search uvar)
      [(? false?)
       (u/r-map:update! uvar (void))
       (let loop ([u/r-cfl u/r-cfl] [reg-cfl '()])
         (match u/r-cfl
           [(? null?)
            (let ([cnd-reg (set-subtract (reg-set) reg-cfl)])
              (if (set-empty? cnd-reg)
                (error 'u/r-cfl->reg! "'~a' has not enough reg" uvar)
                (let ([reg (set-first cnd-reg)])
                  (begin (u/r-map:update! uvar reg) reg))))]
           [(cons u/r next)
            (if (uvar? u/r)
              (match (u/r-cfl->reg! u/r (cfl-graph-find cg u/r) cg)
                [(? void?)
                 (loop next reg-cfl)]
                [|reg|
                 (loop next (cons reg reg-cfl))])
              (loop next (cons u/r reg-cfl)))]))]
      [|v/r|
        (begin v/r)])))

(define cfl-graph-find
  (lambda (cg uvar)
    (let ([found (assq uvar cg)])
      (if (not found)
        (error 'cfl-graph-find "'~a' not found in ~a" uvar cg)
        (cdr found)))))

; ----- test ----- ;
;(apply test assign-registers #:trace #t
;       (list t))
;(test (lambda (x)
;        (hook 
;          ([prev (lambda () (u/r-map:init!))]
;           [post (lambda () (u/r-map:reset!))])
;          (u/r-cfl:analyze! x)
;          (pretty-display (u/r-map:->list))))
;      t)

