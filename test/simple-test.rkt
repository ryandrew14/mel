#lang s-exp "../lib/mel.rkt"

(tempo 80)

(sequence a
          [rest 1]
          [play kick #:repeat: 8]
          [play clap #:rate 0.5])

(sequence b
          [play hihat #:stretch 2])

(song
 [a from 0 to 20]
 [b from 10 to 20])