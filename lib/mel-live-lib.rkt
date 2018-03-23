#lang racket

(require rsound)

(module+ test
  (require rackunit))

(provide
  ; N -> (void)
  ; Update the tempo and beat length
  ; EFFECT sets the tempo and beat length.
  update-tempo

  ; Song -> (void)
  ; play-song runs indefinitely, playing the specified song.
  ; EFFECT plays sounds FOREVER
  play-song

  ; N [Pair Sound [Listof N]] ... -> Loop
  ; Creates a loop object from a list of sounds and which beats to play them on.
  make-loop

  (rename-out
    [s-kick kick]
    [s-snare snare]
    [s-bassdrum bassdrum]
    [s-crash crash]
    [s-hihat hihat]))

;; Data Definitions

; A Song is a [Listof Loop]
; A Loop is a [N -> [Listof Sound]]
; A Sound is one of:
;  - hihat
;  - kick
;  - snare
;  - bassdrum
;  - crash

; INTEPRETATION
; A Song represents the overarching piece of "music"
; A Loop represents a single track of the song. It specifies some sounds for each beat of the song.
; A Sound is the atomic sound data.

; Define scaled sounds to provide
(define s-bassdrum (rs-scale .3 bassdrum))
(define s-kick (rs-scale .3 kick))
(define s-snare (rs-scale .3 snare))
(define s-crash (rs-scale .3 crash-cymbal))
(define s-hihat (rs-scale .3 o-hi-hat))

(define tempo 100)

(define stream (make-pstream))
(define beat-length (* (/ (default-sample-rate) tempo) 60))

; N -> (void)
; Update the tempo and beat length
; EFFECT sets the tempo and beat length.
(define (update-tempo t)
  (set! tempo t)
  (set! beat-length (* (/ (default-sample-rate) tempo) 60)))

; Sound N -> (void)
; Queues a sound onto the stream to play at a certain beat number.
(define (play-at-beat sound beat-num)
  (begin
    (pstream-queue stream sound (round (* beat-length beat-num)))
    (void)))

; Loop N -> (void)
; Queues the sounds of a loop at a specified beat number onto the stream.
(define (play-loop-at-beat loop beat-num)
  (for ([sound (loop beat-num)])
    (play-at-beat sound beat-num)))

; N -> N
; Calculate the beat that the specified frame belongs to.
(module+ test
  (check-equal? (frame->beat 100)
                (floor (/ 100 beat-length))))
(define (frame->beat frame)
  (floor (/ frame beat-length)))

; N -> N
; Calculate the first frame of the specified beat.
(module+ test
  (check-equal? (beat->frame 2)
                (round (* 2 beat-length))))
(define (beat->frame beat)
  (round (* beat beat-length)))

; -> N
; Get the current beat number.
(define (current-beat)
  (frame->beat (pstream-current-frame stream)))

; -> N
; Get the frame number of the next down beat.
(define (next-beat-frame)
  (beat->frame (add1 (current-beat))))

; QUEUE-SIZE defines how many beats to queue at once.
(define QUEUE-SIZE 3)

; QUEUE-AHEAD defines how far in advance to queue sounds, in beats.
(define QUEUE-AHEAD 3)

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
; EFFECT plays sounds FOREVER
(define (play-song song)
  (for ([i QUEUE-SIZE])
    (for ([loop song])
      (play-loop-at-beat loop (+ QUEUE-AHEAD i (current-beat)))))
  (sleep (/ (- (beat->frame (+ 3 (current-beat))) (pstream-current-frame stream)) (default-sample-rate)))
  (play-song song))


; N [Pair Sound [Listof N]] ... -> Loop
; Creates a loop object from a list of sounds and which beats to play them on.
(module+ test
  (define simple (make-loop 4 
                            (list s-bassdrum '(1 2 3)) 
                            (list s-crash '(2 3))))
  (check-equal? (simple 1) (list s-bassdrum))
  (check-equal? (simple 2) (list s-bassdrum s-crash))
  (check-equal? (simple 3) (list s-bassdrum s-crash))
  (check-equal? (simple 4) '()))
(define (make-loop loop-len . sounds)
  (lambda (n)
    (define mod-beat (add1 (modulo n loop-len)))
    (foldr
      (lambda (sound r)
        (define should-play (member mod-beat (second sound)))
        (if should-play
          (cons (first sound) r)
          r))
      '()
      sounds)))

#;
(play-song (list (make-loop 4 
                            (list s-hihat '(1 2 3 4))
                            (list s-snare '(1 2))
                            (list s-bassdrum '(1 2)))))
