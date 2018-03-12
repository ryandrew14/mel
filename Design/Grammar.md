### Grammar Rules

```
Program
     p       = (tempo x) (sequence x [e] ...) ... s

Number
     n       = Number
     
Expressions
     e       = x
             | (play snd play-option ...)
             | (rest n)

Sound
    snd      = kick
             | snare
             | crash
             | hihat
             | clap

Song
     s       = (song song-expr ...)

Play-option
play-option  = #:repeat n
             | #:stretch n
             | #:rate n
             | #:amp n
             | #:attack n
             | #:release n
             | #:sustain n
             | #:pan n

Song-Expression
song-expr    = [e from e to e]
             | [e at e]
```
