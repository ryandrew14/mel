#lang s-exp "../lib/mel-live.rkt"
;#lang racket

(tempo 80)

(define dope '(1 3 4))

(define loop1
  (loop 4
        [bassdrum dope]))


;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play (loop 4 
  [hihat '(2 4)]))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play (loop 5 
  [kick '(2 5)]))

(define a
  (loop 6
    [hihat dope]
    [kick '(1 2 3 4)]))

(play a)


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