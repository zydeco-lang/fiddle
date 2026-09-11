#lang racket/base

(require (only-in racket/unsafe/ops unsafe-car unsafe-cdr))

(provide (struct-out foreign) (struct-out ctype) (struct-out frame) (struct-out vtype) (struct-out tagged)
         regs new-method new-tag matches-tag? Tag
         rkt->fiddle fiddle->rkt fo-rkt->fiddle fo-kw-rkt->fiddle fo-nocheck-rkt->fiddle)

;; The *register file* is a global mutable hash from keywords to
;; values. It is essentially shared global state, and is not
;; saved/restored automatically like the stack is.
(define regs (make-hash))

;; The stack is threaded through every computation as TWO values:
;;
;;   vals   : the values pushed since the last delimiter, most recent
;;            first — which, because application pushes right to left,
;;            is source order: (! f a b c) gives f the segment (a b c).
;;   frames : a list of `frame`s, one per delimiter, innermost first.
;;            Each frame carries the segment that was open when the
;;            delimiter was pushed.
;;
;; frames = '() means the computation is returning to a bind. A
;; delimiter's "arguments" are simply the segment beneath it.
(struct frame (name    ;; a ctype
               vals)   ;; the segment beneath the delimiter
  #:transparent)

(struct vtype (name
               arity)
  #:transparent)

(struct tagged (name
                args)
  #:transparent)

;; A nominal method / stack delimiter. Methods carry no arity: `% m`
;; closes whatever segment is currently open.
(struct ctype (name)   ;; a gensym'd symbol
  #:transparent)

(define (matches-tag? v vty)
  (and (tagged? v)
       (equal? (tagged-name v)
               (vtype-name  vty))))

(define (Tag tag . args)
  (unless (= (length args) (vtype-arity tag))
    (error "tried to construct a tagged value but gave the wrong number of arguments" tag args))
  (tagged (vtype-name tag) args))

(struct foreign (payload))

(define (new-method name)
  (cond [(symbol? name) (ctype (gensym name))]
        [else
         (error "new-method expects a symbol for a name but got" name)]))

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

;; A first-order Racket procedure consumes its arguments and returns a
;; value to whatever is beneath them. If that is a delimiter rather than
;; a bind, the value would be returned into the delimiter — the same
;; error `ret` raises.
(define (delimiter-error frames)
  (error 'fiddle "primitive applied with a delimiter on the stack: ~a"
         (ctype-name (frame-name (car frames)))))

;; wraps first-order, positional-only Racket procedures.
;; The manual implementation for stack length 0-3 is ugly
;; but is better in practice
(define (fo-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ (s frames)
       (unless (null? frames) (delimiter-error frames))
       (cond
         [(null? s) (x)]
         [(null? (unsafe-cdr s))
          (x (unsafe-car s))]
         [(null? (unsafe-cdr (unsafe-cdr s)))
          (x (unsafe-car s) (unsafe-car (unsafe-cdr s)))]
         [(null? (unsafe-cdr (unsafe-cdr (unsafe-cdr s))))
          (x (unsafe-car s) (unsafe-car (unsafe-cdr s)) (unsafe-car (unsafe-cdr (unsafe-cdr s))))]
         [else (apply x s)]))]
    [else (error 'fo-rkt->fiddle-is-for-fo-funs)]))

;; like fo-rkt->fiddle but WITHOUT the delimiter check. Used only for
;; `error`, which is invoked on failure paths exactly when a delimiter
;; is on top and must not have its message masked.
(define (fo-nocheck-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ (s frames) (apply x s))]
    [else (error 'fo-nocheck-rkt->fiddle-is-for-fo-funs)]))

;; wraps first-order Racket procedures that may accept keyword args.
(define (fo-kw-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ (s frames)
       (unless (null? frames) (delimiter-error frames))
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
     (λ (s frames)
       (unless (null? frames) (delimiter-error frames))
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
       (fiddle->rkt (x (map rkt->fiddle args) '())))]))

(module+ test
  (require rackunit)
  (check-equal? (fiddle->rkt #t) #t)
  (check-equal? (rkt->fiddle #t) #t)

  (check-equal? ((fiddle->rkt (rkt->fiddle list)) 1 2 3) '(1 2 3))
  (check-equal? ((fiddle->rkt (rkt->fiddle (λ args (reverse args)))) 1 2 3) '(3 2 1)))
