#lang racket

(require racket/trace)
(require "helper.rkt")
(require "test.rkt")
(provide uncover-register-conflict)

(define uncover-register-conflict
  (lambda (program)
    ($program! program)))

(define $program!
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (let 
          ([cfl-b* (map $body! b*)]
           [cfl-lmd (lambda (l b) `(,l (lambda () ,b)))])
         `(letrec ,(map cfl-lmd l* cfl-b*) ,($body! b)))])))

(define $body!
  (lambda (body)
    (match body
      [`(locals (,u* ...) ,t)
        (hook
          ([prev (lambda () (cfl:push! (cfl:make)))]
           [post (lambda () (cfl:pop!))])
          ($tail! t (live:make))
         `(locals ,u* (register-rconflict ,(cfl:->list) ,t)))])))

(define $tail!
  (lambda (tail live)
    (match tail
      [`(begin ,e* ... ,t)
        (let ([live ($tail! t live)])
          (foldr (lambda (e l) ($effect! e l)) live e*))]
      [`(if ,p ,t-1 ,t-2)
        (let
          ([live-t-2 ($tail! t-2 (live:copy live))]
           [live-t-1 ($tail! t-1 (live:copy live))])
          ($pred! p (live:merge live-t-1 live-t-2)))]
      [`(,t ,l* ...)
        (let* 
          ([fold-deal (lambda (l lv) (if (reg? l) (live:add lv l) lv))]
           [live (foldl fold-deal live l*)])
          ($triv t live))])))

(define $pred!
  (lambda (pred live)
    (match pred
      [`(true)
        (begin live)]
      [`(false)
        (begin live)]
      [`(begin ,e* ... ,p)
        (let ([live ($pred! p live)])
          (foldr (lambda (e l) ($effect! e l)) live e*))]
      [`(if ,p-1 ,p-2 ,p-3)
        (let
          ([live-p-3 ($pred! p-3 (live:copy live))]
           [live-p-2 ($pred! p-2 (live:copy live))])
          ($pred! p-1 (live:merge live-p-2 live-p-3)))]
      [`(,r ,t-1 ,t-2)
        (let*
          ([live ($triv t-1 live)]
           [live ($triv t-2 live)])
          (begin live))])))

(define $effect!
  (lambda (effect live)
    (match effect
      [`(nop)
        (begin live)]
      [`(set! ,v (,op ,t-1 ,t-2))
        (let* 
          ([live ($var! v live)]
           [live ($triv t-1 live)]
           [live ($triv t-2 live)])
          (begin live))]
      [`(set! ,v ,t)
        (let*
          ([live ($var! v live)]
           [live ($triv t live)])
          (when (and (uvar? v) (reg? t))
            (cfl:update! v (live:add (live:make) t)))
          (begin live))]
      [`(if ,p ,e-1 ,e-2)
        (let
          ([live-e-2 ($effect! e-2 (live:copy live))]
           [live-e-1 ($effect! e-1 (live:copy live))])
          ($pred! p (live:merge live-e-1 live-e-2)))]
      [`(begin ,e+ ...)
        (foldr (lambda (e l) ($effect! e l)) live e+)])))

(define $triv
  (lambda (triv live)
    (match triv
      [(? uvar?)
       (live:add live triv)]
      [(? reg?)
       (live:add live triv)]
      [|else|
       (begin live)])))

(define $var!
  (lambda (var live)
    (match var
      [(? uvar?)
       (cfl:update! var live)
       (live:remove live var)]
      [(? reg?)
       (live:remove live var)]
      [|else|
       (begin live)])))

; ----- helper ----- ;
(bundle live ()
  ([make () 
     (set)]
   [add (live val) 
     (set-add live val)]
   [copy (live)
     (set-union live)]
   [merge (live-1 live-2) 
     (set-union live-1 live-2)]
   [remove (live u/r) 
     (if (set-member? live u/r)
       (set-remove live u/r)
       (begin live))]
   [->list (live)
     (set->list live)]
   ))

(bundle cfl ([cfl (void)])
  ([make () 
     (make-hash)]
   [push! (new-cfl)
     (set! cfl (cons new-cfl cfl))]
   [pop! ()
     (set! cfl (cdr cfl))]
   [curr ()
     (car cfl)]
   [set! (uvar live)
     (hash-set! (cfl:curr) uvar live)]
   [find-live-by-uvar (uvar)
     (hash-ref (cfl:curr) uvar #f)]
   [update! (uvar live)
     (let ([u-live (cfl:find-live-by-uvar uvar)])
       (if (not u-live)
         (cfl:set! uvar (live:copy live))
         (cfl:set! uvar (live:merge u-live live))))]
   [->list ()
     (map 
       (lambda (cfl-cons)
         (match cfl-cons
           [(cons uvar live)
            (cons uvar (live:->list (live:remove live uvar)))]))
       (sort (hash->list (cfl:curr)) < #:key (compose set-count cdr)))]
   ))

; ----- test ----- ;
;(apply test uncover-register-conflict #:trace #t
;       ;tests
;       (list t)
;       )

