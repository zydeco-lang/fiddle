#lang fiddle

(require fiddle/prelude)
(require fiddle/prelude)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/IO)
(require "../../Parse.rkt")

;; data Rectangle where
;;   '(rect id ,Num ,Coordinates ,Size)
(define-thunk (! r-coordinates)
  (copat [(r) (! <<v car % vo cdr % vo cdr % vo cdr r % v$)]))
(define-thunk (! r-size)
  (copat [(r) (! <<v car % vo cdr % vo cdr % vo cdr % vo cdr r % v$)]))

;; data Coordinates where
;;   '(coord x ,Nat y ,Nat)
(define-thunk (! c-x)
  (copat [(c) (! <<v car % vo cdr % vo cdr c % v$)]))
(define-thunk (! c-y)
  (copat [(c) (! <<v car % vo cdr % vo cdr % vo cdr % vo cdr c % v$)]))

;; data Size where
;;   '(size width ,Nat height ,Nat)
(define-thunk (! s-width)
  (copat [(s) (! <<v car % vo cdr % vo cdr s % v$)]))
(define-thunk (! s-height)
  (copat [(s) (! <<v car % vo cdr % vo cdr % vo cdr % vo cdr s % v$)]))

(define-thunk (! mk-coord)
  (copat [(x y) (ret (list 'coord 'x x 'y y))]))

(define-thunk (! mk-rect id x y w h)
  (do [c <- (! mk-coord x y)]
      (ret
       (list 'rect 'id id
             c
             (list 'size 'width w 'height h)))))

(define r-ex
  (list 'rect 'id 0
         (list 'coord 'x 3 'y 4)
         (list 'size 'width 8 'height 8)))
(define pt-ex
  (list 'coord 'x 3 'y 4))

;; parse-rect : String -> Rectangle
;; assume it always succeeds
;; Format: #(0-9)+ @ (0-9)+,(0-9)+: (0-9)+x(0-9)+
(define-rec-thunk (! parse-loop)
  (copat
   [(_hash
     (upto idcs #\space)
     _at _spc
     (upto xcs #\,)
     (upto ycs #\:)
     (= #\space)
     (upto wcs #\x))
    (! grab-stack
       (thunk
        (copat
         [(hcs)
          (! <<v apply mk-rect % vo map parse-num (list idcs xcs ycs wcs hcs) % v$)])))]))

(define-thunk (! parse-rect)
  (copat
   [(s)
    (! <<v apply parse-loop % vo string->list s % v$)]))
(define WIDTH  1000)
(define HEIGHT 1000)
(define-thunk (! mk-region)
  (! <<v make-vector % vo * WIDTH HEIGHT % v$))
(define-thunk (! region-ref)
  (copat
   [(r pt)
    (do [x <- (! c-x pt)] [y <- (! c-y pt)]
      [i <- (! <<v + x % vo * WIDTH y % v$)]
      (! vector-ref r i))]))

(define-thunk (! inc-pt)
  (copat
   [(r pt)
    (do [x <- (! c-x pt)] [y <- (! c-y pt)]
      [i <- (! <<v + x % vo * WIDTH y % v$)]
      [v <- (! vector-ref r i)]
      (! <<v vector-set! r i % vo + 1 v % v$))]))

(define-thunk (! coords<-rect)
  (copat
   [(r)
    (do [x <- (! <<v c-x % vo r-coordinates r % v$)]
        [y <- (! <<v c-y % vo r-coordinates r % v$)]
      [w <- (! <<v s-width % vo r-size r % v$)]
      [h <- (! <<v s-height % vo r-size r % v$)]
      [x+ <- (! + x w)]
      [y+ <- (! + y h)]
      (! <<n
         cl-map (thunk (! apply mk-coord)) % no
         cartesian-product (thunk (! range x x+)) (thunk (! range y y+)) % n$))]))

(define-thunk (! colist<-region)
  (copat
   [(r)
    (do [len <- (! * WIDTH HEIGHT)]
        (! <<n
           cl-map (thunk (! vector-ref r))% no 
           range 0 len % n$))]))

(define-thunk (! fill-region)
  (copat
   [(reg rect)
    (! <<n
       cl-foreach (thunk (! inc-pt reg)) % no
       coords<-rect rect % n$)]))

(define-thunk (! main3-1)
  (do [ls <- (! slurp-lines!)]
      [arr <- (! mk-region)]
      [_ <-
         (! <<n
            cl-foreach (thunk (! fill-region arr)) % no
           cl-map parse-rect % no
           colist<-list ls % n$)]
      (! <<n cl-length % no
         cl-filter (thunk (λ (x) (! >= x 2))) % no
         colist<-region arr % n$)))

(define-thunk (! disjoint)
  (copat
   [(reg rect)
    (! <<n
       (thunk (λ (c) (! cl-foldr c and (thunk (ret #t))))) % no
     cl-map (thunk (λ (pt) (ret (thunk  (! <<v equal? 1 % vo region-ref reg pt % v$))))) % no
     coords<-rect rect % n$)]))

(define-thunk (! main3-2)
  (do [ls <- (! slurp-lines!)]
      [arr <- (! mk-region)]
    [rects <- (ret (thunk (! <<n cl-map parse-rect % no colist<-list ls % n$)))]
    [_ <- (! cl-foreach (thunk (! fill-region arr)) rects)]
    (! <<v
       clv-hd % vo 
       cl-filter (thunk (λ (rect) (! disjoint arr rect))) rects % v$)))
;; (! main3-2)
