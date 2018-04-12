#lang racket

(require (prefix-in rs: rsound))
(require "livecoding-test.rkt")

(module+ test
  (require rackunit))

tempo
; QUEUE-SIZE defines how many beats to queue at once.
(define QUEUE-SIZE 4)

; QUEUE-AHEAD defines how far in advance to queue notes, in beats.
(define QUEUE-AHEAD 5)
(define stream #f)
(define beat-length (* (/ (rs:default-sample-rate) tempo) 60))

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
; EFFECT plays notes FOREVER
(define (play-song song)
  (set! stream (rs:make-pstream))
  (for ([i (in-range 1 (add1 QUEUE-AHEAD))])
    (play-song-at-beat song i))
  (let loop ([last-queued QUEUE-AHEAD])
    (define queue-to (+ (current-beat) QUEUE-SIZE QUEUE-AHEAD))
    (for ([i (in-range (+ 1 last-queued) (+ 1 queue-to))])
      (play-song-at-beat song i))
    (sleep (/ (- (beat->frame (+ QUEUE-SIZE (current-beat))) (rs:pstream-current-frame stream))
              (rs:default-sample-rate)))
    (loop queue-to)))



;;; PLAYER LIBRARY

(define-struct note [instrument pitch effects] #:transparent)
(define-struct player [func beats] #:transparent)

; Rsound -> Instrument
; Create a instrument that always return the rnote, scaled.
(define (rs->instrument rs)
  (define s-rs (rs:rs-scale .3 rs))
  (lambda (p)
    s-rs))

(define s-bassdrum (rs->instrument rs:bassdrum))
(define s-kick (rs->instrument rs:kick))
(define s-snare (rs->instrument rs:snare))
(define s-crash (rs->instrument rs:crash-cymbal))
(define s-hihat (rs->instrument rs:o-hi-hat))

; Song N -> (void)
; Queues all notes in a song for a given beat.
(define (play-song-at-beat song beat)
  (for ([player song])
    (play-player-at-beat player beat)))

; Note -> Rsound
; Turns the note struct into an rsound
(module+ test
  (define a-bassdrum (note s-bassdrum 1 (list identity)))
  (check-equal? (note->rs a-bassdrum)
                (s-bassdrum 1)))
(define (note->rs s)
  ((apply compose (note-effects s))
   ((note-instrument s) (note-pitch s))))

; Note N -> (void)
; Queues a note onto the stream to play at a certain beat number.
; Beat-num should NOT be 0! 
; The pstream plays upon creation, so it is impossible to hear notes at beat 0.
(define (play-at-beat snd beat-num)
  (if (positive? beat-num)
      (begin
        (rs:pstream-queue stream (note->rs snd) (beat->frame beat-num))
        (void))
      (error "Cannot play notes at beat-num 0"))) 

; Player N -> (void)
; Queues the notes of a loop at a specified beat number onto the stream.
(define (play-player-at-beat p beat-num)
  (if ((player-func p) beat-num)
      (play-at-beat ((player-func p) beat-num) beat-num)
      (void)))

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
  (frame->beat (rs:pstream-current-frame stream)))

; -> N
; Get the frame number of the next down beat.
(define (next-beat-frame)
  (beat->frame (add1 (current-beat))))

