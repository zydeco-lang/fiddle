#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require "../../Parse.rkt")
(require fiddle/stdlib/CoList)
(provide main-a main-b)

(define-thunk (! log x)
  (do [_ <- (! displayln x)] (ret x)))

;; data FreeGroup A = List (Polar A) where there are no adjacent terms (+ x) (- x) or (- x) (+ x)
;; data Polar A = `(,Sign ,A)
;; data Sign = '+ or '-

; Letter -> FreeGroup UpperLetter
(define-thunk (! parse-atom)
  (copat
   [(c)
    (cond
      [(! upper-case? c) (ret (list '+ c))]
      [#:else (do [C <- (! char-upcase c)] (ret (list '- C)))])]))

(define-thunk (! starts-with? l x)
  (! and
     (~ (! <<v not % vo empty? l % v$))
     (~ (! <<v equal? x % vo car l % v$))))

(define-thunk (! polar-op p)
  (do [sgn <- (! first p)]
      [sgn-op <- (ifc (! equal? '+ sgn) (ret '-) (ret '+))]
    (! <<v List sgn-op % vo second p % v$)))

; FreeGroup A -> Polar A -> FreeGroup A
(define-thunk (! free-act s x)
  (cond
    [(! <<v starts-with? s % vo polar-op x % v$) (! cdr s)]
    [#:else (ret (cons x s))]))

(define-thunk (! main-a)
  (do [reduced <- (! <<n
                     cl-foldl^ free-act '() % no
                     cl-map parse-atom % no
                     cl-filter letter? % no
                     read-all-chars % n$)]
      (! length reduced)))

(define-thunk (! remove-and-reduce)
  (copat
   [(un-reduced c)
    (do [reduced <- (! <<n
                       cl-foldl^ free-act '() % no
                       cl-filter (~ (λ (x) (! <<v not % vo equal? c % vo second x % v$))) % no
                       colist<-list un-reduced % n$)]
        (! <<v log % vo List c % vo length reduced % v$))]))

;; just did a visual inspection: the smallest answer is much smaller
;; so it's obvious

;; since removing a letter is a homomorphism (just deciding to send an
;; element and its inverse to the identity), you can reuse the reduced
;; word from part a.
(define-thunk (! main-b)
  (do [reduced <- (! <<n
                     cl-foldl^ free-act '() % no
                     cl-map parse-atom % no
                     cl-filter letter? % no
                     read-all-chars % n$)]
      [cs*counts
       <-
       (! <<n
          list<-colist % no
          cl-map (~ (! remove-and-reduce reduced)) % no
          (~ (! <<v colist<-list % vo string->list UPPERS % v$)) % n$)]
    (ret 'done)))
