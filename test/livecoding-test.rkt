#lang mel

(tempo 280)

(define loop-a (loop 8 (player hihat '(1 2 3 4))))

loop-a

(loop 8 (player kick '(1 5 6)))

(loop 4 (player snare '(3)))

(loop 16 (pitch D#maj '(1 5 7 2) (player synth '(13 14 15 16))))

(loop 2 (pitch D#maj '(1) (player synth '(1))))
