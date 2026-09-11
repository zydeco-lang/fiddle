#lang fiddle
;; The Fiddle counterpart of racket-idiomatic.rkt: the same zipper
;; algorithm as solution.rkt (two lists, reverse on wraparound) written
;; as a plain loop — no colists, no message-dispatching circle object,
;; scores in a vector. `move-right` hands its three results to a
;; continuation on the stack instead of allocating a list for them.
(require fiddle/prelude)
(provide main main-b)

(define NUM-PLAYERS 470)
(define NUM-MARBLES-B 7217000)

;; move-right : List A -> List A -> Nat -> U(List A -> A -> List A -> B) -> B
(def-thunk (! move-right l r n k)
  (cond [(! null? r) [r <- (! reverse l)] (! move-right '() r n k)]
        [(! zero? n) [x <- (! car r)] [r <- (! cdr r)] (! k l x r)]
        [else [x <- (! car r)] [r <- (! cdr r)] [n <- (! - n 1)]
              (! move-right (cons x l) r n k)]))

(def-thunk (! next-player p)
  [p <- (! + p 1)]
  (! modulo p NUM-PLAYERS))

(def-thunk (! vector-max-loop v len i m)
  (cond [(! >= i len) (ret m)]
        [else [s <- (! vector-ref v i)]
              [m <- (ifc (! >= m s) (ret m) (ret s))]
              [i <- (! + i 1)]
              (! vector-max-loop v len i m)]))
(def-thunk (! vector-max v)
  [len <- (! vector-length v)]
  (! vector-max-loop v len 0 -inf.0))

;; loop : Vector Nat -> Nat -> Nat -> List Nat -> Nat -> List Nat -> Nat -> F Nat
(def-thunk (! loop scores n marble l x r p)
  (cond
    [(! > marble n) (! vector-max scores)]
    [(! <<v zero? % vo modulo marble 23)
     ;; move -7 = move-right 7 in the mirrored zipper, then swap the sides
     (! move-right r (cons x l) 7
        (~ (λ (r2 x2 l2)
             ;; remove the current marble: the new current is the first of r
             (! move-right l2 r2 0
                (~ (λ (l3 x3 r3)
                     (do [s <- (! vector-ref scores p)]
                         [s <- (! + marble x2 s)]
                         (! vector-set! scores p s)
                         [marble <- (! + marble 1)]
                         [p <- (! next-player p)]
                         (! loop scores n marble l3 x3 r3 p))))))))]
    [else
     (! move-right l (cons x r) 2
        (~ (λ (l2 x2 r2)
             (do [marble+1 <- (! + marble 1)]
                 [p <- (! next-player p)]
                 (! loop scores n marble+1 l2 marble (cons x2 r2) p)))))]))

(def-thunk (! main n)
  [scores <- (! make-vector NUM-PLAYERS 0)]
  (! loop scores n 1 '() 0 '() 0))
(def-thunk (! main-b) (! main NUM-MARBLES-B))

(do [x <- (! main-b)] (! displayln x))
