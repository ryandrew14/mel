#lang mel

(tempo 280)

(play (loop 8 (player hihat '(1 2 3 4))))

(play (loop 8 (player kick '(1 5 6))))

(play (loop 4 (player snare '(3))))

(play (loop 16 (pitch D#maj '(1 5 7 2) (player synth '(13 14 15 16)))))

(play (loop 2 (pitch D#maj '(1) (player synth '(1)))))
