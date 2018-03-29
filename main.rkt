#lang racket
;;   Program      = (tempo N) top-expr ...
;;
;;   top-expr     = (play expr)
;;                | (define id expr)
;;
;;    expr        = N
;;                | (list N ...)
;;                | player-expr
;;                | id
;;
;;  player-expr     = (player N sound-expr ...)
;;
;; sound-expr     = [sound (list N ...)]
;;
;;  sound         = hihat
;;                | kick
;;                | snare
;;                | bassdrum
;;                | crash

;; TODO
;; make quote raise a better exception
;; add error messages to #%datum

(require (for-syntax syntax/parse)

		 "lib/mel-live-lib.rkt")

(provide
  ; Override module begin
  (rename-out [mel-module-begin #%module-begin]
			  [mel-datum #%datum]
			  [mel-quote quote]
			  [player-from-instrument player]
			  [set-loop loop]
			  [set-pitches pitch]
			  [set-reverb reverb])

  ; Racket basics
  define require #%app

  ; Macros
  play 

  ; Library things
  bassdrum hihat kick snare crash synth

  ; Keys
  Cmaj C#maj Dmaj D#maj Emaj Fmaj F#maj Gmaj G#maj Amaj A#maj Bmaj 
  Cmin C#min Dmin D#min Emin Fmin F#min Gmin G#min Amin A#min Bmin) 

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
		(play-song cursong))]))

;; Syntax -> Void
;; EFFECT plays this sound at a given time
(define-syntax (play stx)
  (if (equal? (syntax-local-context) 'module)
	(syntax-parse stx
	  [(_ n:nat)
	   (error "invalid syntax - play must take a player-expression")]
	  [(_ (list l:nat ...))
	   (error "invalid syntax - play must take a player-expression")]
	  [(_ player-expr)
	   #:with fin-player #'player-expr
	   #'(update-song fin-player)])
	#'(error "Play is a top level form!")))

;; Restrict available datatypes to numbers, lists of numbers, and identifiers
(define-syntax mel-datum
  (syntax-parser
	[(_ . x:nat)
	 #'(#%datum . x)]
	[(_ x:id)
	 #'(#%datum x)]
	[(_ #`(list #,x:nat ...))
	 #'(#%datum (list x ...))]))

;; Mel's quote can only make lists of numbers
(define-syntax mel-quote
  (syntax-parser
	[(_ (n:number ...))
	 #'(quote (n ...))]))


;; Runtime helper to create the song
(define (update-song player)
  (set! cursong (cons player cursong))) 

(module reader syntax/module-reader
  mel
  #:read
  read
  #:read-syntax
  read-syntax)
