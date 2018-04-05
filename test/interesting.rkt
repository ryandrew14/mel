#lang mel

(tempo 80)

; Wrong size for list of pitches.
#;
(play (pitch midi '(60 62) (player synth '(1 2 4))))


; Pitch after loop, so that pitch change only happens the first time.
; This is a feature.
#;
(play (pitch midi '(60 62 64) 
             (loop 4
                   (pitch '() '(72 74 76)
                          (player synth '(1 2 4))))))

; Looping a measure that is shorter than the last beat of a player-object
; just cuts out the beats that aren't in the loop interval.
#;
(play (loop 4 (player hihat '(1 4 5))))  ; Plays looping 4 beat measure with hihat on beats 1 and 4

; Duplicate and out-of-order beats
#;
(play (player hihat '(1 2 2 2 1 4 3)))  ; Plays hihat on beats 1, 2, 3, and 4

; Pitching percussion instruments
#;
(play (pitch midi '(60 61 62) (player snare '(1 2 3)))) ; Pitch function has no effect