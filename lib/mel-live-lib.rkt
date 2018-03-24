#lang racket

(require rsound)

(module+ test
  (require rackunit))

(provide
  (contract-out
    ; Update the tempo and beat length
    ; EFFECT sets the tempo and beat length.
    [update-tempo (-> integer? void?)]

    ; play-song runs indefinitely, playing the specified song.
    ; EFFECT plays sounds FOREVER
    [play-song (-> song? void?)]

    ; N [Pair Sound [Listof N]] ... -> Loop
    ; Creates a loop object from a list of sounds and which beats to play them on.
    [make-loop (->* (integer?) () #:rest (cons/c rsound? (listof integer?)) loop?)])

  (rename-out
    [s-kick kick]
    [s-snare snare]
    [s-bassdrum bassdrum]
    [s-crash crash]
    [s-hihat hihat]))

; Data Definitions

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


;; Constants

; Define scaled sounds to provide
(define s-bassdrum (rs-scale .3 bassdrum))
(define s-kick (rs-scale .3 kick))
(define s-snare (rs-scale .3 snare))
(define s-crash (rs-scale .3 crash-cymbal))
(define s-hihat (rs-scale .3 o-hi-hat))

(define tempo 100)

(define stream #f)
(define beat-length (* (/ (default-sample-rate) tempo) 60))

;; Predicates

; Any -> Boolean?
; Is this a song?
(define (song? s)
  (andmap loop?  s))

; Any -> Boolean?
; Is this a loop?
(define (loop? a)
  (and (procedure? a) (= 1 (procedure-arity a))))

; N -> (void)
; Update the tempo and beat length
; EFFECT sets the tempo and beat length.
(define (update-tempo t)
  (set! tempo t)
  (set! beat-length (* (/ (default-sample-rate) tempo) 60)))

; Sound N -> (void)
; Queues a sound onto the stream to play at a certain beat number.
; Beat-num should NOT be 0! 
; The pstream plays upon creation, so it is impossible to hear sounds at beat 0.
(define (play-at-beat sound beat-num)
  (if (positive? beat-num)
    (begin
      (pstream-queue stream sound (beat->frame beat-num))
      (void))
    (error "Cannot play sounds at beat-num 0")))


; Loop N -> (void)
; Queues the sounds of a loop at a specified beat number onto the stream.
(define (play-loop-at-beat loop beat-num)
  (for ([sound (loop beat-num)])
    (play-at-beat sound beat-num)))

; N -> N
; Calculate the beat that the specified frame belongs to.
; Frame 0 corresponds to beat 0. Frame <beat-length> is beat 1.
; There should NOT be anything playing at beat 0!
(module+ test
  (check-equal? (frame->beat 100)
                (floor (/ 100 beat-length))))
(define (frame->beat frame)
  (floor (/ frame beat-length)))

; N -> N
; Calculate the first frame of the specified beat.
(module+ test
  (check-equal? (beat->frame 1)
                (round beat-length))
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
(define QUEUE-SIZE 5)

; QUEUE-AHEAD defines how far in advance to queue sounds, in beats.
(define QUEUE-AHEAD 3)

; Song N -> (void)
; Queues all sounds in a song for a given beat.
(define (play-song-at-beat song beat)
  (for ([loop song])
    (play-loop-at-beat loop beat)))

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
; EFFECT plays sounds FOREVER
(define (play-song song)
  (define (loop)
    (for ([i QUEUE-SIZE])
      (play-song-at-beat song (+ QUEUE-AHEAD i (current-beat))))
    (sleep (/ (- (beat->frame (+ QUEUE-SIZE (current-beat))) (pstream-current-frame stream)) (default-sample-rate)))
    (loop))
  (set! stream (make-pstream))
  (for ([i (in-range 1 (add1 QUEUE-AHEAD))])
    (play-song-at-beat song i))
  (loop))


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
    (define mod-beat (add1 (modulo (sub1 n) loop-len)))
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
