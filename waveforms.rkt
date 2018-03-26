#lang racket
(require rsound)

(define snum 3)

(define stream (make-pstream))

(for ([i (in-range 1 100)])
  (for ([b (in-range (* i 4) (* (add1 i) 4))]
        [midi '(0 2 4 5)])
    (pstream-queue stream (synth-note "main" i (+ midi 60) (/ (default-sample-rate) 2)) (* b (/ (default-sample-rate) 2)))))
