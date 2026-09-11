#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require (except-in fiddle/stdlib/CoList take))
(require "../../Stream.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(define WIDTH 25)
(define HEIGHT 6)

(def-thunk (! slurp-input)
  [chars <- (! <<v string->list % vo first % vo slurp-lines! % v$)]
  (ret (~ (! <<n cl-map digit<-char % no colist<-list chars % n$))))

(def-thunk (! summarize xs)
  [step
   = (~ (λ (summary)
          (do [zeros <- (! first summary)]
              [ones <- (! second summary)]
            [twos <- (! third summary)]
            (copat [((= 0)) [zeros <- (! + 1 zeros)]
                            (! List zeros ones twos)]
                   [((= 1)) [ones <- (! + 1 ones)]
                            (! List zeros ones twos)]
                   [((= 2)) [twos <- (! + 1 twos)]
                            (! List zeros ones twos)]))))]
  (! cl-foldl xs step '(0 0 0)))
(def-thunk (! summarize! xs) (! summarize (~ (! colist<-list xs))))

(def-thunk (! main-a)
  [nums <- (! slurp-input)]
  [layer-size <- (! * WIDTH HEIGHT)]
  [zs*ones*twos <- (! <<n minimum-by first '(+inf.0 0 0) % no cl-map summarize! % no chunks layer-size nums % n$)]
  [ones <- (! second zs*ones*twos)]
  [twos <- (! third zs*ones*twos)]
  (! * ones twos))

(def/copat (! atop-pixel)
  [((= 2) inner) (ret inner)]
  [(outer inner) (ret outer)])

(def-thunk (! apply-layer l1 l2)
  (! <<n list<-colist
     % no cl-map (~ (! apply atop-pixel))
     % no cl-zipwith (~ (! colist<-list l1)) (~ (! colist<-list l2))))

;; print-row : Listof Number -> F 1
(def-thunk (! print-row ns)
  [pixel->string
   = (~ (copat
         [((= 0)) (ret " ")]
         [((= 1)) (ret "#")]))]
  (! <<v displayall % vo apply string-append % vo map pixel->string ns))

(def-thunk (! main-b)
  [nums <- (! slurp-input)]
  [layer-size <- (! * WIDTH HEIGHT)]
  [final-layer <- (! <<n cl-foldl1 apply-layer % no chunks layer-size nums % n$)]
  (! <<n cl-foreach print-row % no chunks WIDTH (~ (! colist<-list final-layer)))
  )
