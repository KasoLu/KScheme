#lang racket

(require racket/trace)
(require "helper.rkt")
(provide discard-call-live)

(define discard-call-live
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (let ([dis-lmd (lambda (l b) `(,l (lambda () ,b)))])
          `(letrec ,(map dis-lmd l* (map $body b*)) ,($body b)))])))

(define $body
  (lambda (body)
    (match body
      [`(locate ,bind* ,t)
       `(locate ,bind* ,($tail t))])))

(define $tail
  (lambda (tail)
    (match tail
      [`(if ,p ,t-1 ,t-2)
       `(if ,p ,($tail t-1) ,($tail t-2))]
      [`(begin ,e* ... ,t)
       `(begin ,@e* ,($tail t))]
      [`(,t ,_ ...)
       `(,t)])))

; ----- test ----- ;
;(test discard-call-live #:trace #t
;      t)

