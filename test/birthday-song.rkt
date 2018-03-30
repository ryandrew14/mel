#lang mel

(tempo 160)

;; Plays a happy birthday tune!

(play (pitch Cmaj '(5 5 6 5 8 7 5 5 6 5 9 8 5 5 13 12 8 7 6 11 11 10 8 9 8)
             (player synth2 '(1 2 3 5 7 9 13 14 15 17 19 21
                               25 26 27 29 31 33 35 39 40 41 43 45 47))))
