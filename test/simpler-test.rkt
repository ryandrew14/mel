#lang s-exp "../lib/mel.rkt"

;; raco cmdline tool
;; raco make
;; raco exe
;; Jay McCarthy RacketCon 2016 15 (ask Leif; fast bb playstation emulator)

(tempo 160)

(sequence bd
          [play bassdrum]
          [rest]
          [rest]
          [rest]
          [play bassdrum]
          [play bassdrum]
          [rest]
          [play bassdrum])
          
          #|[rest]
          [rest]
          [rest]
          [rest]
          [play bassdrum]
          [play bassdrum]
          [rest]
          [play bassdrum])|#

(sequence mid
          [play snare]
          [play snare]
          [rest]
          [rest])

(sequence bot
          [play bassdrum]
          [play bassdrum]
          [rest]
          [rest])

(sequence b
          [play hihat]
          [play kick]
          [play snare]
          [play bassdrum]
          [play crash])


(song
 [bd at 1])