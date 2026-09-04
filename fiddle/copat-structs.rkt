#lang racket/base

;; Struct representations for the copat matcher in prelude.rkt.
;; Wrapped and exposed as Fiddle values from fiddle.rkt.

(provide (struct-out arg-copat)
         (struct-out upto-copat)
         (struct-out upto-multi-copat)
         (struct-out kw-copat)
         (struct-out method-copat)
         (struct-out lit-pat)
         (struct-out cons-pat)
         (struct-out list-pat)
         (struct-out method-pat)
         (struct-out method-only)
         (struct-out upto-syn)
         (struct-out upto-multi-syn)
         (struct-out kw-syn)
         (struct-out method-syn))

;; Runtime copat forms — output of view-copat, consumed by copat-match.
;; Each has a `rest` field pointing to the remainder of the pattern list
;; (which is itself a struct or 'any-stack / 'rest / end-copat).
(struct arg-copat    (pat rest)     #:transparent)
(struct upto-copat   (sigil rest)   #:transparent)
;; upto-multi-copat: multi-sigil upto. `sigils` is a Fiddle list of literal
;; values; `end?` is #t if end-of-stack is also a valid terminator. When
;; matched, match-k receives the args-before-sigil and the found sigil
;; value (or the keyword #:bind for end-of-stack).
(struct upto-multi-copat (sigils end? rest) #:transparent)
(struct kw-copat     (kw pat rest)  #:transparent)
(struct method-copat (m pat rest)   #:transparent)

;; Pat-syntax forms — emitted by the copat macro, consumed by view-copat.
;; These describe one pattern-list element without the "rest" tail.
(struct upto-syn   (sigil)  #:transparent)
(struct upto-multi-syn (sigils end?) #:transparent)
(struct kw-syn     (kw pat) #:transparent)
(struct method-syn (m pat)  #:transparent)

;; Argument-pattern forms — used inside arg-copat's `pat` field, inside
;; method-copat's `pat` field, and (for lit-pat / method-pat / method-only)
;; inside upto-copat's `sigil` field.
(struct lit-pat     (v)       #:transparent)
(struct cons-pat    (car cdr) #:transparent)
(struct list-pat    (ps)      #:transparent)
(struct method-pat  (m pat)   #:transparent)
(struct method-only (m)       #:transparent)
