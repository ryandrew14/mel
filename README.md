# mel
Music Embedding Language for racket. #lang mel

### Grammar Rules

```racket
; Program 
; p = (tempo x) (sequence x e) ... s

; Expressions
; e = x
;   | N
;   | (e from e to e)
;   | (e at e)
;   | (play x)
;   | (rest e)

; Stream
; s = (stream [e] ...)

```

### Scoping Rules

- (sequence a e) binds a for the rest of the program (not in e)
