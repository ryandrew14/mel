# Livecoding Grammar Rules

```
(tempo 80)

(define dope '(1 3 4))

;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play (loop 4 
  [hihat '(2 4)]))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play (loop 5 
  [kick '(2 5)]))

(define a
  (loop 6
    [hihat dope]
    [kick '(1 2 3 4)]))

(play a)
```
```
   Program      = (tempo N) top-expr ...

   top-expr     = (play expr)
                | (define id expr)

    expr        = N
                | (list N ...)
                | loop-expr
                | id

  loop-expr     = (loop N sound-expr ...)

 sound-expr     = [sound (list N ...)]

  sound         = hihat
                | kick
                | snare
                | bassdrum
                | crash

```
