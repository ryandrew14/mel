### Grammar Rules

```
Program
     p       = (tempo x) (sequence x [e] ...) ... s

Expressions
     e       = x
             | N
             | (play e play-option ...)
             | (rest N)
             | kick
             | snare
             | crash
             | hihat
             | clap

Song
     s       = (song song-expr ...)

Play-option
play-option  = #:repeat number
             | #:stretch number
             | #:rate number
             | #:amp number
             | #:attack number
             | #:release number
             | #:sustain number
             | #:pan number

Song-Expression
song-expr    = [e from e to e]
             | [e at e]
```
