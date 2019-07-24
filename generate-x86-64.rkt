#lang racket

(require "helper.rkt")
(provide generate-x86-64)

(define generate-x86-64
  (lambda (program)
    (match program
      [`(code ,s* ...)
        (display boilerplate-start)
        (for-each (compose display stmt->x86-64) s*)
        (display boilerplate-end)])))

(define stmt->x86-64
  (lambda (stmt)
    (match stmt 
      [`(set! ,l-t (,o ,t-1 ,t-2))
        (format "  ~a ~a, ~a\n" 
                (binop->x86-64 o) 
                (triv->x86-64 t-2) 
                (loc->x86-64 l-t))]
      [`(set! ,(? reg? l-t) ,(? label? l))
        (format "  leaq ~a(%rip), ~a\n"
                (label->x86-64 l)
                (loc->x86-64 l-t))]
      [`(set! ,l-t ,t)
        (format "  movq ~a, ~a\n"
                (triv->x86-64 t)
                (loc->x86-64 l-t))]
      [`(if (,r ,t-1 ,t-2) (jump ,l))
        (format
          "  cmpq ~a, ~a\n  ~a ~a\n"
          (triv->x86-64 t-2)
          (triv->x86-64 t-1)
          (relop->x86-64 r)
          (label->x86-64 l))]
      [`(if (not (,r ,t-1 ,t-2)) (jump ,l))
        (format 
          "  cmpq ~a, ~a\n  ~a ~a\n"
          (triv->x86-64 t-2)
          (triv->x86-64 t-1)
          (relop->x86-64 (not-relop r))
          (label->x86-64 l))]
      [`(jump ,t)
        (format
          (if (label? t)
            "  jmp ~a\n"
            "  jmp *~a\n")
          (triv->x86-64 t))]
      [ (? label? l)
        (format "~a:\n" (label->x86-64 l))]
      )))

(define triv->x86-64
  (lambda (triv)
    (match triv
      [(? loc?)
       (loc->x86-64 triv)]
      [(? int?)
       (format "$~a" triv)]
      [(? label?)
       (label->x86-64 triv)]
      )))

(define loc->x86-64
  (lambda (loc)
    (match loc
      [(? reg?)
       (format "%~a" loc)]
      [(disp-opnd reg offset)
       (format "~a(%~a)" offset reg)]
      )))

(define label->x86-64
  (lambda (label)
    (match (label-match label)
      [`(,_ ,(app string->number idx))
        (format "L~a" idx)])))

(define binop->x86-64
  (lambda (binop)
    (match binop
      ['+ "addq"]
      ['- "subq"]
      ['* "imulq"]
      ['logand "andq"]
      ['logor "orq"]
      ['sra "sarq"])))

(define relop->x86-64
  (lambda (relop)
    (match relop
      ['=  "je"]
      ['>  "jg"]
      ['<  "jl"]
      ['!= "jne"]
      ['>= "jge"]
      ['<= "jle"])))

(define not-relop
  (lambda (relop)
    (match relop
      ['=  '!=]
      ['>  '<=]
      ['<  '>=]
      ['!= '=]
      ['>= '<]
      ['<= '>])))

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

; ----- test ------ ;
;(test generate-x86-64
;      '(code
;         (set! r8 3) 
;         (set! r9 10) 
;         f$1 
;         (if (not (= r8 1)) (jump a$9)) 
;         c$8 
;         (jump c$6) 
;         a$9 
;         (if (not (> r9 1000)) (jump a$7)) 
;         c$6 
;         (set! rax r9) 
;         (jump r15) 
;         a$7 
;         (set! r9 (* r9 2)) 
;         (set! rax r8) 
;         (set! rax (logand rax 1)) 
;         (if (not (= rax 0)) (jump a$4)) 
;         c$3 
;         (set! r9 (+ r9 1)) 
;         (jump j$5) 
;         a$4 
;         j$5 
;         (set! r8 (sra r8 1)) 
;         (jump f$1)))

;(test generate-x86-64
;      '(code
;         (set! r8 3)
;         (set! r9 10)
;         f$1
;         (if (= r8 1) (jump l$1015))
;         l$1016
;         (if (not (> r9 1000)) (jump l$1012))
;         l$1015
;         (set! rax r9)
;         (jump r15)
;         l$1012
;         (set! r9 (* r9 2))
;         (set! rax r8)
;         (set! rax (logand rax 1))
;         (if (not (= rax 0)) (jump l$1003))
;         l$1007
;         (set! r9 (+ r9 1))
;         l$1003
;         (set! r8 (sra r8 1))
;         (jump f$1)))
;(test generate-x86-64 #:trace #t
;      '(code
;         l$104
;         (set! rax 5)
;         l$103
;         (set! rcx 10)
;         l$102
;         (if (< rax rcx) (jump r15))
;         l$101
;         (set! rax rcx)
;         (jump r15)))

