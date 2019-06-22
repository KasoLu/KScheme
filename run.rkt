#lang racket

(require "helper.rkt")
(require "verify-scheme.rkt")
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
    debug-program))

(define build-passes
  (list
    verify-scheme
    finalize-locations
    expose-frame-var
    expose-basic-blocks
    inline-basic-blocks
    flatten-program
    generate-x86-64
    ))

(driver debug-passes build-passes invalid-tests)
(driver debug-passes build-passes tests)



