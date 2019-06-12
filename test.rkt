#lang racket

(provide (all-defined-out))

(define tests
  '((begin
      (set! rax 8)
      (set! rcx 3)
      (set! rax (- rax rcx)))))

