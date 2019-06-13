#lang racket

(require "helper.rkt")
(require "verify-scheme.rkt")
(require "expose-frame-var.rkt")
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
    expose-frame-var
    flatten-program
    generate-x86-64))

(driver debug-passes build-passes invalid-tests)
(driver debug-passes build-passes tests)

