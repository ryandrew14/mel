#lang s-exp "../lib/mel.rkt"

(tempo 160)

(sequence a
          [play kick]
          [play clap])

(sequence b
          [play hihat])

(song
 [a at 0]
 [b at 2])