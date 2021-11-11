#lang racket

(define-syntax-rule (parameterize_rhom a b body)
  (parameterize ([a b]) body))

(provide parameterize_rhom)
