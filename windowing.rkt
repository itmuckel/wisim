#lang typed/racket

(require typed/racket/gui/base)
(require "logic.rkt")

; -------- state
(: resources (HashTable Material Integer))
(define resources (hash
                   'iron-ore 100000
                   'steel 0
                   ))

(define iron-mine 
  (corp 
   "iron-mine" 
   (production (list (production-material 'iron-ore 2 'tons))
               (list (production-material 'steel 2 'tons))) 
   2))

(define corporations (list iron-mine))

; -------- windowing
(define frame (new frame% [label "wisim"]))

(define label-iron-ore (new message% 
                            [parent frame]
                            [label "iron-ore"]
                            [auto-resize #t]))
(define label-steel (new message% 
                         [parent frame]
                         [label "steel"]
                         [auto-resize #t]))

(define last-tick (current-milliseconds))
(define process-rate 33)

(define (process)
  (define dt (- (current-milliseconds) last-tick))
  (when (> dt process-rate) 
    (set! last-tick (current-milliseconds))
    (set! resources (update-materials resources corporations))
    (send label-steel set-label
          (string-append "steel: " (number->string (hash-ref resources 'steel)) " tons"))
    (send label-iron-ore set-label
          (string-append "iron-ore: " (number->string (hash-ref resources 'iron-ore)) " tons"))
    ))

;; TODO: this procedure should return the updated materials instead of setting global state.

(new timer% 
     [notify-callback (λ () (process))]
     [interval 1000])

(define corp-list-box (new list-box% 
                           [label #f] 
                           [parent frame] 
                           [choices (map corp-name corporations)]))
