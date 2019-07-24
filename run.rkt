#lang racket

(require "helper.rkt")
(require "verify-scheme.rkt")
(require "uncover-register-conflict.rkt")
(require "assign-registers.rkt")
(require "discard-call-live.rkt")
(require "finalize-locations.rkt")
(require "expose-frame-var.rkt")
(require "expose-basic-blocks.rkt")
(require "inline-basic-blocks.rkt")
(require "flatten-program.rkt")
(require "generate-x86-64.rkt")
(require "debug-program.rkt")
(require "test.rkt")

(define debug-passes
  (list
    verify-scheme
    uncover-register-conflict
    assign-registers
    discard-call-live
    debug-program
    ))

(define build-passes
  (list
    verify-scheme
    uncover-register-conflict
    assign-registers
    discard-call-live
    finalize-locations
    expose-frame-var
    expose-basic-blocks
    inline-basic-blocks
    flatten-program
    generate-x86-64
    ))

;(driver debug-passes build-passes invalid-tests)
;(driver debug-passes build-passes tests)

;(test (apply pipe debug-passes) #:trace #t t)
;(test (apply pipe build-passes) #:trace #t t)

