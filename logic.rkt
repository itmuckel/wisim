#lang typed/racket

(provide (all-defined-out))

(define-type Resources (HashTable Material Integer))
(define-type Unit (U 'tons 'liters))
(define-type Material (U 'iron-ore 'steel))

(struct production-material 
  ([name : Material] [quantity : Integer] [unit : Unit]))

(struct production
  ([input : (Listof production-material)] 
   [output : (Listof production-material)]))

(struct corp 
  ([name : String] 
   [production : production]
   [price : Integer]))

(: update-materials (-> Resources (Listof corp) Resources))
(define (update-materials resources corps)
  (for/fold ([res resources]) ([corporation corps])
    (process-corp res corporation)))

(: process-corp (-> Resources corp Resources))
(define (process-corp resources corporation)
  (match-define (corp _ (production input-list
                                    output-list) _) corporation)
  (cond 
    ; Can the product be produced?
    [(andmap (λ (material)
               (match-define (production-material type quantity _) material)
               (resources-available? resources type quantity)) 
             input-list)
     ; Remove all input products
     (: reduced-resources Resources)
     (define reduced-resources 
       (for/fold ([res resources]) ([input input-list])
         (match-define (production-material type quantity _) input)
         (define input-available (hash-ref resources type))
         (hash-set res type (- input-available quantity))))
     ; Add all output products 
     (for/fold ([res reduced-resources]) ([output output-list])
       (match-define (production-material type quantity _) output)
       (define output-available (hash-ref resources type))
       (hash-set res type (+ output-available quantity)))]
    [else resources]))

(: resources-available? (-> Resources Material Integer Boolean))
(define (resources-available? resources type quantity)
  (define input-available (hash-ref resources type))
  (> input-available quantity))
