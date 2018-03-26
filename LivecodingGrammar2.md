# Livecoding Grammar Rules

```
(tempo 80)

(define a-rhythm '(1 3 4))

;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play (loop 4
  [hihat '(2 4)]))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play (loop 5
  [kick '(2 5)]))

(define a
  (amp '(2 4 1)
    (loop 6
      [hihat a-rhythm]
      [kick '(1 2 3 4)])))

(play a)
```
```
   Program      = (tempo N) top-expr ...

   top-expr     = (play expr)
                | (define id expr)

    expr        = N
                | (list N ...)
                | id
                | player-expr

    sound       = hihat
                | kick
                | snare
                | bassdrum
                | crash

  player-expr   = (amp N player-expr)
				| (loop N player-expr)
                | (player sound (list N ...))


```
