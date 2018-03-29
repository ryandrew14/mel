# Livecoding Grammar Rules

```
(tempo 80)

(define a-rhythm '(1 3 4))

;; Play a looping 4-beat measure which plays a hi-hat sound
;; on beats 2 and 4
(play
  (loop 4 (player hihat '(2 4))))

;; Play a looping 5-beat measure which plays a kick sound
;; on beats 2 and 4
(play 
  (loop 5 (player kick '(2 4))))

;; Defines a, a looping 6-beat measure which plays a hihat
;; sound at beats 1, 3, and 4
(define a
    (loop 6 hihat a-rhythm))

;; Play a
(play a)

;; Define a basic synth playing the notes C and G on
;; beats 2 and 4
(define basic-synth
  (pitch '(1 5) (player synth '(2 4))))

;; Play basic-synth
(play basic-synth)

```
```
   Program      = (tempo N) top-expr ...

   top-expr     = (define id expr)
				| (play player-expr)

    expr        = N
                | (list N ...)
				| player-expr
                | id

  player-expr   = (player instrument (list N ...))
				| (pitch Key-name (list N ...) player-expr)
				| (loop N player-expr)
				| (reverb player-expr)
				| id 

  instrument    = hihat
                | bassdrum
                | snare
                | kick
                | crash
				| synth
```
