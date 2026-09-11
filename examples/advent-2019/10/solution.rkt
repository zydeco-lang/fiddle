#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)

;; data:
;;
;; 1. the width and height of the map
;; 2. the starmap, a function (n <= width) x (m <= height) -> Bool

;; A coordinate n m is a List {<= n} {<= m}

(def-thunk (! change-of-origin new-origin c)
  (! idiom^ (~ (! coord-add c)) (~ (! scale new-origin -1))))

(def-thunk (! unchange-of-origin new-origin c)
  (! coord-add new-origin c))

(def-thunk (! magnitude coord)
  [x <- (! x-coord coord)] [y <- (! y-coord coord)]
  (! gcd x y))

(def-thunk (! unitize coord)
  [scaler <- (! magnitude coord)]
  (! <<v scale coord % vo / 1 scaler % v$))

;; Coordinate n m -> Colist (Coordinate n m)
(def-thunk (! obstructions coord)
  [scaler <- (! magnitude coord)] [unit <- (! unitize coord)]
  (! <<n cl-map (~ (! scale unit)) % no range 1 scaler % n$))

;; A starmap n m is a thunk supporting
;; 'width : F Nat
;; 'height : F Nat
;; 'asteroid? : Coordinate n m -> F Bool
;; 'vaporize! : Coordinate n m -> F 1

(def-thunk (! pt<-ix w h ix)
  (! idiom^ mk-coord (~ (! modulo ix w)) (~ (! quotient ix w))))

;; x y |-> y * width + x
(def-thunk (! ix<-pt w h pt)
  [x <- (! x-coord pt)] [y <- (! y-coord pt)]
  (! <<v + x % vo * y w % v$))

;; Implement a starmap backed by a vector of size width * height
(def/copat (! from-vec width height v)
  [((= 'width)) (ret width)]
  [((= 'height)) (ret height)]
  [((= 'asteroid?) pt)
   (! <<v vector-ref v % vo ix<-pt width height pt % v$)]
  [((= 'vaporize!) pt)
   [ix <- (! ix<-pt width height pt)]
   (! vector-set! v ix #f)])

(def-thunk (! in-bounds? smap pt)
  (! and
     (~ (! idiom^ <  (~ (! x-coord pt)) (~ (! smap 'width))))
     (~ (! idiom^ >= (~ (! x-coord pt)) (~ (ret 0))))
     (~ (! idiom^ <  (~ (! y-coord pt)) (~ (! smap 'height))))
     (~ (! idiom^ >= (~ (! y-coord pt)) (~ (ret 0))))))

;; F (Starmap)
(def-thunk (! slurp-map)
  [lines <- (! slurp-lines!)]
  [width <- (! <<v length % vo string->list % vo first lines % v$)]
  [height <- (! length lines)]
  [v <- (! <<n (~ (! .v list->vector list<-colist)) % no
           cl-map (~ (! equal? #\#)) % no
           cl-join % no
           cl-map (~ (λ (xs) (ret (~ (! colist<-list xs))))) % no
           cl-map string->list % no
           colist<-list lines % n$)]
  (ret (~ (! from-vec width height v))))

;; idea: use relative coordinates, if a candidate's relative
;; coordinates x, y have a common divisor, then check the shit that's
;; closer first to see if it's blocked.

;; To start let's do a slow implementation: just for each square check
;; every other square to see if it sees something there.
(def-thunk (! sees? starmap src tgt)
  [relative-tgt <- (! change-of-origin src tgt)]
  (! <<n
     (~ (! .v not any?)) % no
     cl-map Thunk % no
     cl-map (~ (! starmap 'asteroid?)) % no
     cl-map (~ (! unchange-of-origin src)) % no
     obstructions relative-tgt % n$))

;; all-asteroids : Starmap -> CoList Coordinate
(def-thunk (! all-asteroids smap)
  [w <- (! smap 'width)] [h <- (! smap 'height)]
  [len <- (! * w h)]
  (! <<n
     cl-map (~ (! pt<-ix w h)) % no
     cl-filter (~ (λ (i)
                    (do [pt <- (! pt<-ix w h i)]
                        (! smap 'asteroid? pt)))) % no
     range 0 len % n$))

(def-thunk (! all-seen smap pt)
  (! <<n
     cl-length % no
     cl-filter (~ (! sees? smap pt)) % no
     cl-filter (~ (! <<v not % vo equal? pt)) % no
     all-asteroids smap % n$))

(def-thunk (! main-a)
  [smap <- (! slurp-map)]
  (! <<n
     minimum-by (~ (! <<v * -1 % vo second)) '(0 -inf.0) % no
     cl-map (~ (λ (pt) (! <<v List pt % vo all-seen smap pt % v$))) % no
     all-asteroids smap % n$))

(def-thunk (! safe-/ x y)
  (cond [(! zero? y) (! * x +inf.0)]
        [else (! / x y)]))

;; pre-cond not both = 0

;; return an angle from 0 to 2pi
(def-thunk (! better-angle z)
  [bad-angle <- (! angle z)]
  (cond [(! < bad-angle 0) (! idiom^ + (~ (! * 2 pi)) (~ (ret bad-angle)))]
        [else (ret bad-angle)]))
;; polar coordinates as fraction of the way around a circle of unit circumference
;; 0 is at x = 0, y = -max

(def-thunk (! anglify x y)
  [complex <- (! idiom^ (~ (! + x)) (~ (! * y 0+1i)))]
  [rotated <- (! * complex 0+1i)]
  (! better-angle rotated))

(def-thunk (! angle-< c1 c2)
  (! idiom^ < (~ (! apply anglify c1)) (~ (! apply anglify c2))))

(def-thunk (! all-angles smap pt)
  [angles-with-dups <- (! <<n
                          list<-colist % no
                          cl-map unitize % no
                          cl-map (~ (! change-of-origin pt)) % no
                          cl-filter (~ (! <<v not % vo equal? pt)) % no
                          all-asteroids smap % n$)]
  [angles-unsorted <- (! <<v set->list % vo list->set angles-with-dups % v$)]
  (! sort angles-unsorted angle-<))

(def-thunk (! attempt-to-vaporize smap pt k)
  (cond [(! smap 'asteroid? pt)
         (! smap 'vaporize! pt)
         (ret pt)]
        [else (! k)]))

;; vaporize-one : Starmap -> Coordinate -> Unit-Coordinate -> F(Union #f Coordinate)
(def-thunk (! vaporize-one smap src angle)
  (! <<n
     cl-foldr^ (~ (! attempt-to-vaporize smap)) (~ (ret #f)) % no
   take-while (~ (! in-bounds? smap)) % no
   cl-map (~ (! unchange-of-origin src)) % no
   cl-map (~ (! scale angle)) % no
   range 1 +inf.0 % n$)
  )

;; vaporize-loop : Starmap -> Coordinate -> U(CoList Coordinate) -> CoList Coordinate
(def-thunk (! vaporize-loop smap src targets)
  [vaporize-step
   = (~ (λ (tgt later-vaporized)
          (do 
              [may-coord <- (! vaporize-one smap src tgt)]
              (if may-coord
                  (! cl-cons may-coord later-vaporized)
                  (! later-vaporized)))))]
  (! cl-foldr targets vaporize-step cl-nil))

;; sample-b start is 8 3
;; sample-4 start is 11 13
;; part a answer  is 8 16
(def-thunk (! main-b)
  [smap <- (! slurp-map)]
  [src <- (! mk-coord 8 16)]
  [angle-list <- (! all-angles smap src)]
  [vaporized-asteroids = (~ (! <<n vaporize-loop smap src % no cl-cycle (~ (! colist<-list angle-list)) % n$))]
  (! <<n cl-foreach displayall % no cl-zipwith vaporized-asteroids % no range 1 201 % n$))

;; cartesian x y to polar is
;; theta = tan^-1(y / x)
;; so comparing angle(x y) <= angle(x' y') is the same as
;;   y/x <= y'/x' 
