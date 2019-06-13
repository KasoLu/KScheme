#lang racket

(require "helper.rkt")
(provide generate-x86-64)

(define generate-x86-64
  (lambda (program)
    (display boilerplate-start)
    (program->x86-64 program)
    (display boilerplate-end)))

(define program->x86-64
  (lambda (program)
    (match program
      [`(code ,stmt* ...)
        (for-each (compose display stmt->x86-64) stmt*)])))

(define stmt->x86-64
  (lambda (stmt)
    (match stmt
      [`(set! ,var (,binop ,triv-1 ,triv-2))
        (format "  ~a ~a, ~a\n" (binop->x86-64 binop) (triv->x86-64 triv-2) (var->x86-64 var))]
      [`(set! ,var ,triv)
        (if (label? triv)
          (format "  leaq ~a(%rip), ~a\n" (triv->x86-64 triv) (var->x86-64 var))
          (format "  movq ~a, ~a\n" (triv->x86-64 triv) (var->x86-64 var)))]
      [`(jump ,triv)
        (format
          (if (label? triv)
            "  jmp ~a\n"
            "  jmp *~a\n")
          (triv->x86-64 triv))]
      [ (? label?)
        (format "~a:\n" (triv->x86-64 stmt))])))

(define triv->x86-64
  (lambda (triv)
    (match triv
      [(? var?)
       (var->x86-64 triv)]
      [(? int?)
       (format "$~a" triv)]
      [(? label?)
       (format "L~a" (label->index triv))])))

(define var->x86-64
  (lambda (var)
    (match var
      [(? reg?)
       (format "%~a" var)]
      [(? disp-opnd?)
       (format "~a(%~a)" (disp-opnd-offset var) (disp-opnd-reg var))])))

(define binop->x86-64
  (lambda (binop)
    (match binop
      ['+ "addq"]
      ['- "subq"]
      ['* "imulq"]
      ['logand "andq"]
      ['logor "orq"]
      ['sra "sarq"])))

; ----- helper ----- ;
(define boilerplate-start "
  .globl _scheme_entry

_scheme_entry:
  pushq %rbx
  pushq %rbp
  pushq %r12
  pushq %r13
  pushq %r14
  pushq %r15
  movq %rdi, %rbp
  leaq _scheme_exit(%rip), %r15
")

(define boilerplate-end "
_scheme_exit:
  popq %r15
  popq %r14
  popq %r13
  popq %r12
  popq %rbp
  popq %rbx
  ret
")

; ----- test ----- ;
;(test generate-x86-64
;      '(code
;         (jump r15)
;         main$0
;         (set! rax 10)
;         (jump main$0)
;         main$1
;         (set! rbx 20)
;         (jump main$1)))
;(test generate-x86-64
;      `(code
;         (set! rax 17)
;         (jump f$1)
;         f$1
;         (set! ,(disp-opnd 'rbp 0) rax)
;         (set! rax (+ rax rax))
;         (set! rax (+ rax ,(disp-opnd 'rbp 0)))
;         (jump r15)))

