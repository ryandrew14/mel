#lang s-exp "../lib/mel-live.rkt"

(tempo 80)

; Wrong size for list of pitches.
;(play (pitch '() '(60 62 64) (player synth '(1 2 4))))


; Pitch after loop, so that pitch change only happens the first time.
; This is a feature.
(play (pitch '() '(60 62 64) 
             (loop 4
                   (pitch '() '(72 74 76)
                          (player synth '(1 2 4))))))
