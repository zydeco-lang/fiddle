#lang racket/base

;; Error-path tests for copattern matching, driven from rackunit.
;; The Fiddle side lives in an inner module; a Fiddle thunk is a 1-arg
;; Racket procedure over the stack (a list), so we can call it directly.

(module m fiddle
  (require fiddle/prelude)
  (provide bad zero-clauses)
  (define bad (thunk (copat [((= 1)) (ret 'one)])))
  (define zero-clauses (thunk (copat))))

(require 'm rackunit)

(check-equal? (bad '(1)) 'one)
(check-exn #rx"copattern-match-error" (λ () (bad '(2))))
;; the message must mention the remaining args
(check-exn #rx"\\(2\\)" (λ () (bad '(2))))
(check-exn #rx"copattern-match-error" (λ () (zero-clauses '())))
