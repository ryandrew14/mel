#lang racket


(require reloadable)


; QUEUE-SIZE defines how many beats to queue at once.
(define QUEUE-SIZE 5)

; QUEUE-AHEAD defines how far in advance to queue notes, in beats.
(define QUEUE-AHEAD 20)

(define play-the-song (reloadable-entry-point->procedure
                       (make-reloadable-entry-point 'play-the-song "livecoding-test.rkt")))
  
(define current-beat (reloadable-entry-point->procedure
                      (make-reloadable-entry-point 'current-beat "livecoding-test.rkt")))

(define (play-song)
  (let loop ([last-queued 0])
    (define queue-to (+ (current-beat) QUEUE-SIZE QUEUE-AHEAD))
    (define need-to-queue (< last-queued queue-to))
    (displayln (format
                "The current beat is ~a, we have queued up to ~a"
                (current-beat)
                last-queued))
    (if need-to-queue
        (for ([i (in-range (+ 1 last-queued) (+ 1 queue-to))])
          (play-the-song i))
        #f)
    (sleep 3)
    (loop (max last-queued queue-to))))

(reload!)

(play-the-song 3)