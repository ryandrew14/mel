# MEL
Music Embedding Language for racket. #lang mel

### A basic use example
```
#lang mel
 
(tempo 80)
 
(define seqA '(1 2 3 4))
(define seqB '(1 2))
 
(define loop1
(loop 4
      (player hihat seqB)))
 
(play loop1)
 
(play (loop 4 (player hihat seqA)))
 
(play (loop 4 (reverb (player snare seqB))))
 
(play (loop 4 (pitch Cmaj '(1 2 3 5) (player synth '(1 2 3 4)))))
```
