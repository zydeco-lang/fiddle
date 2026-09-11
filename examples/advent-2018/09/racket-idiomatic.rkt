#lang racket/base
;; Plain-Racket reference point for solution.rkt (part b).
;; Same zipper algorithm (two lists, reverse on wraparound), written as a
;; plain loop: no lazy streams, no message dispatch, direct vector scores.
(define NUM-PLAYERS 470)
(define NUM-MARBLES-B 7217000)
(define (move-right l r n)
  (cond [(null? r) (move-right '() (reverse l) n)]
        [(zero? n) (values l (car r) (cdr r))]
        [else (move-right (cons (car r) l) (cdr r) (- n 1))]))
(define (main n)
  (define scores (make-vector NUM-PLAYERS 0))
  (let loop ([marble 1] [l '()] [x 0] [r '()] [p 0])
    (cond
      [(> marble n) (for/fold ([m -inf.0]) ([s (in-vector scores)]) (if (>= m s) m s))]
      [(zero? (modulo marble 23))
       ;; move -7 = move-right 7 in the mirrored zipper, then swap sides
       (define-values (r2 x2 l2) (move-right r (cons x l) 7))
       ;; remove current: the new current is the first of the right list
       (define-values (l3 x3 r3) (move-right l2 r2 0))
       (vector-set! scores p (+ marble x2 (vector-ref scores p)))
       (loop (+ marble 1) l3 x3 r3 (modulo (+ p 1) NUM-PLAYERS))]
      [else
       (define-values (l2 x2 r2) (move-right l (cons x r) 2))
       (loop (+ marble 1) l2 marble (cons x2 r2) (modulo (+ p 1) NUM-PLAYERS))])))
(displayln (main NUM-MARBLES-B))
