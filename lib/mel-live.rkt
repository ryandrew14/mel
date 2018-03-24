#lang racket
;;   Program      = (tempo N) top-expr ...
;;
;;   top-expr     = (play expr)
;;                | (define id expr)
;;
;;    expr        = N
;;                | (list N ...)
;;                | loop-expr
;;                | id
;;
;;  loop-expr     = (loop N sound-expr ...)
;;
;; sound-expr     = [sound (list N ...)]
;;
;;  sound         = hihat
;;                | kick
;;                | snare
;;                | bassdrum
;;                | crash

(require (for-syntax syntax/parse)
         
	 "mel-live-lib.rkt")

(provide
 ; Override module begin
 (rename-out [mel-module-begin #%module-begin])
 

 ; Racket basics
 define quote #%datum #%top-interaction require play loop

 ; Library things
 bassdrum hihat kick)

;; The song
(define cursong '())

(define-syntax mel-module-begin
  (syntax-parser
    [(_ ((~datum tempo) t)
        top-expr ...)
     #'(#%module-begin
        (update-tempo t)
        top-expr ...
        (displayln cursong)
        (play-song cursong))]
    [(_) (error "Program does not follow grammar: (tempo x) (sequence x [seq-expr] ...) ... s")]))

;; Syntax -> Void
;; EFFECT plays this sound at a given time
(define-syntax play
  (syntax-parser
    [(_ n:nat)
     (error "invalid syntax - play must take a loop-expression")]
    [(_ (list l:nat ...))
     (error "invalid syntax - play must take a loop-expression")]
    [(_ loop-expr)
     #:with fin-loop #'loop-expr
     #'(update-song fin-loop)]))

;; Syntax -> [N -> [Listof Sound]]
;; given a loop-expr, uses the library function make-loop to format the information
;; so that it can be used by the library function play-song
(define-syntax loop
  (syntax-parser
    [(_ n:nat [sound-name lon] ...)
     #'(make-loop n (list sound-name lon) ...)]))

;; Restrict available datatypes to numbers, lists of numbers, and identifiers
(define-syntax mel-#%datum
  (syntax-parser
    [(_ . x:nat)
     #'(#%datum . x)]
    [(_ x:id)
     #'(#%datum x)]
    [(_ #`(list #,x:nat ...))
     #'(#%datum (list x ...))]))

(define (update-song loop)
  (set! cursong (cons loop cursong)))








