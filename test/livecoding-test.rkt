#lang s-exp "../lib/mel-live.rkt"

(tempo 80)

;(define a (tempo 2))
(define dope '(1 3 4))

#|
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

|#

