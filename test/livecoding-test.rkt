#lang s-exp "../lib/mel-live.rkt"
;#lang racket

(tempo 460)
#|
(define dope '(0 1))

(define loop1
  (loop 4
        (player bassdrum dope)))

(define aaa '(1 1))

;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play (loop 4 
  (player hihat '(2 4))))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play (loop 5 
  (player kick '(2 5))))

(define a
  (loop 6
    (player kick '(1 2 3 4))))

(play a)
|#

(define seqA '(1 2 3 4))
(define seqB '(1 2))

(define loop1
  (loop 4
        (player hihat seqB)))

(play loop1)

;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play (loop 4 
            (player hihat seqA)))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play (loop 5 
            (player snare seqB)))

#|
(module testguy racket
  (provide
   (rename-out [d #%datum])
   #%module-begin
   #%top
   #%app
   #%top-interaction
   lambda)
  (require (for-syntax syntax/parse))
  (define-syntax d
    (syntax-parser
      [(_ . n:nat)
       #'(#%datum . n)])))

(module ok (submod ".." testguy)
  10)

(require 'ok)
|#
