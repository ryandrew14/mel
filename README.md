# MEL
Music Embedding Language for racket. #lang mel

### A basic use example
```
#lang mel

(tempo 80)

(sequence a
  [play kick #:repeat: 8]
  [play clap #:rate 0.5])

(sequence b
  [hihat #:stretch 2])

(song
  [a from 0 to 20]
  [b from 10 to 20])
```
