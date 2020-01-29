#lang racket
(require redex)

; Grammar for our model of Elastic Work Stealing
; ----------------------------------------------

(define-language Elastic-Work-Stealing

  ($ ::=                            ; costs:
     (natural                       ;   work
      natural))                     ;   span
  (g ::=                            ; series-parallel graphs:
     $                              ;   cost (result)
     •                              ;   vertex
     (∘ g g)                        ;   serial composition
     (∥ g g))                       ;   parallel composition
  (E ::=                            ; evaluation contexts:
     hole
     (∘ E g)
     (∘ $ E)
     (∥ E g)
     (∥ $ E))

  (j ::=                            ; join identifiers
     variable-not-otherwise-mentioned) 
  (p ::= P1 P2)                     ; join-target positions
  (j+ ::=                           ; join targets:
      ·                             ;   terminal
      (p j))                        ;   target join j from position t
  ($+ ::=                           ; optional costs:
      ·                             ;   pending cost
      $)                            ;   cost
  (jr ::=                           ; join records:
      (j                            ;   identifier
       $+                           ;   storage for result of first completed branch
       E                            ;   local continuation
       j+))                         ;   join continuation
  (Eg ::= (E g))                    ; context + term
  (W ::=                            ; workers:
     (Eg j+)                        ;   busy computing Eg, with continuation join j+
     S                              ;   stealing
     Z)                             ;   sleeping
  (J ::= (jr ...))                  ; join record environments
  (M ::= (W ... J ...)))            ; machine configurations
  
; Metafunctions
; -------------

(define-metafunction Elastic-Work-Stealing
  ⊕-∘ : $ $ -> $
  [(⊕-∘
    (natural_w1 natural_s1)
    (natural_w2 natural_s2))
   (,(+ (term natural_w1) (term natural_w2) 1)
    ,(+ (term natural_s1) (term natural_s2) 1))])

(define-metafunction Elastic-Work-Stealing
  ⊕-∥ : $ $ -> $
  [(⊕-∥
    (natural_w1 natural_s1)
    (natural_w2 natural_s2))
   (,(+ (term natural_w1) (term natural_w2) 1)
    ,(+ (max (term natural_s1) (term natural_s2)) 1))])

(define-metafunction Elastic-Work-Stealing
  ⊕-∥+ : j+ $ $ -> $ or #f
  [(⊕-∥+ (P1 _) $_1 $_2) (⊕-∥ $_1 $_2)]
  [(⊕-∥+ (P2 _) $_1 $_2) (⊕-∥ $_2 $_1)]
  [(⊕-∥+ _ _ _) #f])

(define-metafunction Elastic-Work-Stealing
  Try-to-split-E : E -> (E E g) or #f
  [(Try-to-split-E (∘ E g))
   (E_c (∘ E_1 g) g_2)
   (where (E_c E_1 g_2) (Try-to-split-E E))]
  [(Try-to-split-E (∘ $ E))
   (E_c (∘ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E))]
  [(Try-to-split-E (∥ E g))
   (E hole g)]
  [(Try-to-split-E (∥ $ E))
   (E_c (∥ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E))]
  [(Try-to-split-E _) #f])

; Reduction relation
; ------------------

(define →
  (reduction-relation
   Elastic-Work-Stealing #:domain M

   ; Single-threaded steps
   ; ---------------------
   
   (s--> (E •) (E (1 1)) "•-$")
   (s--> ((∘ $_1 E) $_2) (E (⊕-∘ $_1 $_2)) "∘-↑")
   (s--> ((∥ $_1 E) $_2) (E (⊕-∥ $_1 $_2)) "∥-↑")
   (s--> ((∘ E g_2) $_1) ((∘ $_1 E) g_2) "∘-→")
   (s--> ((∥ E g_2) $_1) ((∥ $_1 E) g_2) "∥-→")
   (s--> (E (∘ g_1 g_2)) ((∘ E g_2) g_1) "∘-↓")
   (s--> (E (∥ g_1 g_2)) ((∥ E g_2) g_1) "∥-↓")

   ; Multithreaded steps
   ; -------------------
   
   (--> (W_b ... ((E g) j+_v) W_m ... S W_a ... (jr ...))
        (W_b ... ((E_1 g) (P1 j)) W_m ... ((hole g_t) (P2 j)) W_a ... (jr_f jr ...))
        (where (E_c E_1 g_t) (Try-to-split-E E))
        (fresh j)
        (where jr_f (j · E_c j+_v))
        "Steal-1")

   ; later: make stea rule symmetric

   (--> (W_b ... ((hole $) (_ j)) W_a ... (jr_b ... (j · E j+) jr_a ...))
        (W_b ... S W_a ... (jr_b ... (j $ E j+) jr_a ...))
        "Join-1")

   (--> (W_b ... ((hole $_1) (p j)) W_a ... (jr_b ... (j $_2 E j+) jr_a ...))
        (W_b ... ((E $) j+) W_a ... (jr_b ... jr_a ...))
        (where $ (⊕-∥+ (p j) $_1 $_2))
        "Join-2")

   with
   [(--> (W_b ... (Eg1 j+) W_a ... J)
         (W_b ... (Eg2 j+) W_a ... J))
    (s--> Eg1
          Eg2)]))

; Unit tests
; ----------

(define-metafunction Elastic-Work-Stealing
  g→$ : g -> $
  [(g→$ •)
   (1 1)]
  [(g→$ (∘ g_1 g_2))
   (⊕-∘ $_1 $_2)
   (where $_1 (g→$ g_1))
   (where $_2 (g→$ g_2))]
  [(g→$ (∥ g_1 g_2))
   (⊕-∥ $_1 $_2)
   (where $_1 (g→$ g_1))
   (where $_2 (g→$ g_2))])

(define-metafunction Elastic-Work-Stealing
  Mk-M0 : g natural -> M
  [(Mk-M0 g natural)
   (((hole g) (P1 j0)) W ... ((j0 · hole ·)))
   (where (W ...) ,(build-list (term natural) (λ _ (term S))))])

(define g1
  (term
   (∥ (∘ • •) (∘ • •))))

(define M1
  (term (Mk-M0 ,g1 2)))

(define-metafunction Elastic-Work-Stealing
  Get-ans : M -> $ or #f
  [(Get-ans (W ... ((j0 $ hole ·)))) $]
  [(Get-ans _) #f])

(define equiv-M-$
  (λ (M $)
    (equal? (term (Get-ans ,M)) $)))

(test-->> → #:equiv equiv-M-$
          M1
          (term (g→$ ,g1)))