#lang racket/base

(provide (struct-out foreign) (struct-out ctype) (struct-out method) (struct-out vtype) (struct-out tagged)
         regs new-method matches-method? invoke-method new-tag matches-tag? Tag
         rkt->fiddle fiddle->rkt fo-rkt->fiddle fo-kw-rkt->fiddle)

;; The runtime state.
;;
;; The *stack* is no longer a global mutable cell — it is now an ordinary
;; list threaded as the (only) argument to every Fiddle computation. Each
;; Fiddle computation compiles to a Racket procedure `(λ (stack) …)` and
;; returns the value that the eventual `ret` produces.
;;
;; A stack is a list of Methods where each element is one of
;; - a plain value (an argument pushed on)
;; - a `method` struct (a nominal method frame with its args and remaining tail)
;;
;; The *register file* remains a global mutable hash from keywords to
;; values. `^:` writes it and `kw-case-λ` reads/removes from it.
(define regs (make-hash))

(struct vtype (name
               arity)
  #:transparent)

(struct tagged (name
                args)
  #:transparent)

(struct ctype (name    ;; a gensym'd symbol
               arity)  ;; a natural number
  #:transparent
  )

(struct method (name ;; a gensym'd symbol
                args ;; a list of arguments
                tl)  ;; the rest of the methods
  #:transparent)

(define (matches-tag? v vty)
  (and (tagged? v)
       (equal? (tagged-name v)
               (vtype-name  vty))))

(define (matches-method? methods cty)
  (and (method? methods)
       (equal? (method-name methods)
               (ctype-name  cty))))

(define (Tag tag . args)
  (unless (= (length args) (vtype-arity tag))
    (error "tried to construct a tagged value but gave the wrong number of arguments" tag args))
  (tagged (vtype-name tag) args))

;; invoke-method : stack ctype -> stack
;; Take the top `arity` values off `meths` and pack them into a
;; method struct on top of the remaining tail. Purely functional.
(define (invoke-method meths cty)
  (define (loop meths remaining args)
    (cond [(zero? remaining)
           (method (ctype-name cty) (reverse args) meths)]
          [(pair? meths)
           (define hd (car meths))
           (define meths^ (cdr meths))
           (loop meths^ (sub1 remaining) (cons hd args))]
          [else
           (error "tried to apply a method but didn't get enough arguments")]))
  (unless (ctype? cty)
    (error "tried to apply something that wasn't a method: " cty))
  (loop meths (ctype-arity cty) '()))

(struct foreign (payload))

(define (new-method name arity)
  (cond [(and (symbol? name)
              (exact-nonnegative-integer? arity))
         (ctype (gensym name) arity)]
        [else
         (error "new-method expects a symbol for a name and natural number for arity but got" name arity)]))

(define (new-tag name arity)
  (cond [(and (symbol? name) (exact-nonnegative-integer? arity))
         (vtype (gensym name) arity)]
        [else
         (error "new-tag expects a symbol for a name and natural number for arity but got" name arity)]))

(define (fiddle-datum? x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (symbol? x)
      (null? x)
      (char? x)
      (keyword? x)
      (struct? x)))

(define (regs->kvs)
  (define kvs (sort (hash->list regs)
                    keyword<?
                    #:key car))
  (values (map car kvs) (map cdr kvs)))

;; wraps first-order, positional-only Racket procedures. Skips the regs
;; hash-count check and keyword-apply fallback that fo-kw-rkt->fiddle
;; needs — those cost about half of runtime on primop-heavy hot paths.
;; Specializes on stack length 0..3 to avoid the apply-time arity check
;; on the hot arithmetic path (Racket's +, -, *, <, etc. are variadic,
;; so apply always has to length-walk).
(define (fo-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ (s)
       (cond
         [(null? s) (x)]
         [(null? (cdr s)) (x (car s))]
         [(null? (cddr s)) (x (car s) (cadr s))]
         [(null? (cdddr s)) (x (car s) (cadr s) (caddr s))]
         [else (apply x s)]))]
    [else (error 'fo-rkt->fiddle-is-for-fo-funs)]))

;; wraps first-order Racket procedures that may accept keyword args.
;; Drains regs into a keyword-apply on every call.
(define (fo-kw-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ (s)
       (cond [(zero? (hash-count regs))
              (apply x s)]
             [else
              (define-values (ks vs) (regs->kvs))
              (hash-clear! regs)
              (keyword-apply x ks vs s)]))]
    [else (error 'fo-kw-rkt->fiddle-is-for-fo-funs)]))

;; racket value -> fiddle value
(define (rkt->fiddle x)
  (cond
    [(fiddle-datum? x) x]
    [(pair? x) (cons (rkt->fiddle (car x)) (rkt->fiddle (cdr x)))]
    [(procedure? x)
     (λ (s)
       (cond [(zero? (hash-count regs))
              (rkt->fiddle (apply x (map fiddle->rkt s)))]
             [else
              (define-values (ks vs) (regs->kvs))
              (hash-clear! regs)
              (rkt->fiddle
               (apply x ks (map fiddle->rkt vs) (map fiddle->rkt s)))]))]
    [else (foreign x)]))

;; fiddle->rkt
(define (fiddle->rkt x)
  (cond
    [(fiddle-datum? x) x]
    [(pair? x) (cons (fiddle->rkt (car x))
                     (fiddle->rkt (cdr x)))]
    [(foreign? x) (foreign-payload x)]
    [(procedure? x)
     (λ args
       (fiddle->rkt (x (map rkt->fiddle args))))]))

(module+ test
  (require rackunit)
  (check-equal? (fiddle->rkt #t) #t)
  (check-equal? (rkt->fiddle #t) #t)

  (check-equal? ((fiddle->rkt (rkt->fiddle list)) 1 2 3) '(1 2 3))
  (check-equal? ((fiddle->rkt (rkt->fiddle (λ args (reverse args)))) 1 2 3) '(3 2 1)))
