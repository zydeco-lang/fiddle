#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")
(require fiddle/stdlib/FlexVec)

(provide main-a main-b)

;; assumptions: all programs end in 99

;; the object program is self-modifying so we can't really abstract over
;; the syntax: just make it a list of integers



;; parse-chars
;; Char ->* F (Listof Number)
;; todo: develop a mo-fucking parsing library!
(def/copat (! parse-chars)
  [((= 'loop) nums digits (= #\newline))
   [num <- (! <<v parse-num % vo reverse digits % v$)]
   (! <<v reverse % vo Cons num nums % v$)]
  [((= 'loop) nums digits (= #\,))
   [num <- (! <<v parse-num % vo reverse digits % v$)]
   [nums <- (! Cons num nums)]
   (! parse-chars 'loop nums '())]
  [((= 'loop) nums digits c)
   [digits <- (! Cons c digits)]
   (! parse-chars 'loop nums digits)]
  [() (! parse-chars 'loop '() '())])

(def-thunk (! operation memory op src1 src2 dest)
  [f <- (cond [(! equal? op 1) (ret +)]
              [(! equal? op 2) (ret *)]
              [else (ret (~
                          (do (! displayall op)
                              (! error "invalid-opcode"))))])]
  [inp1 <- (! memory 'get src1)]
  [inp2 <- (! memory 'get src2)]
  [outp <- (! f inp1 inp2)]
  (! memory 'set dest outp))

(def/copat (! eval-opcodes)
  [((= 'loop) memory i #:bind)
   [code <- (! memory 'get i)]
   ((copat
     [((= 99)) (ret 'done)]
     [(op)
      [src1 <- (! <<v memory 'get % vo + i 1 % v$)]
      [src2 <- (! <<v memory 'get % vo + i 2 % v$)]
      [dest <- (! <<v memory 'get % vo + i 3 % v$)]
      (! operation memory op src1 src2 dest)
      (! <<v eval-opcodes 'loop memory % vo + i 4 % v$)])
    code)]
  [(memory #:bind)
   (! eval-opcodes 'loop memory 0)])

(def-thunk (! parse-opcodes)
  [chars <- (! <<n list<-colist % no read-all-chars % n$)]
  (! apply parse-chars chars))

(def-thunk (! run-opcode-program opcodes inp1 inp2)
  [memory <- (! mutable-flexvec<-list opcodes)]
  (! memory 'set 1 inp1)
  (! memory 'set 2 inp2)
  (! eval-opcodes memory)
  (! memory 'get 0))

(def-thunk (! main-a)
  [opcodes <- (! parse-opcodes)]
  (! run-opcode-program opcodes 12 2))

(define EXPECTED-OUTPUT 19690720)

(def-thunk (! check-solution opcodes args k)
  ;; (! displayall 'trying args)
  (! apply
     (~ (copat [(inp1 inp2)
                [outp <- (! run-opcode-program opcodes inp1 inp2)]
                (cond [(! = outp EXPECTED-OUTPUT)
                       (! displayall 'gotem inp1 inp2)]
                      [else (! k)])]))
     args))

(def-thunk (! main-b)
  [opcodes <- (! parse-opcodes)]
  [inps = (~ (! cartesian-product (~ (! range 0 100)) (~ (! range 0 100))))]
  (! cl-foldr inps (~ (! check-solution opcodes))
                   (~ (! error "no solution found!"))))
