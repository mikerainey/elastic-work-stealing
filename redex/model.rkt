#lang racket
(require redex)

; Series-Parallel Graph language
; ------------------------------
;
; A language for expressing series-parallel graphs and
; their costs as specified by the work-span cost model.

(define-language SP-Graph
  (g ::=                            ; series-parallel graphs:
     •                              ;   vertex
     (∘ g g)                        ;   serial composition
     (∥ g g))                       ;   parallel composition
  ($ ::=                            ; costs:
     (natural                       ;   work
      natural)))                    ;   span

(define-metafunction SP-Graph
  ⊕-∘ : $ $ -> $
  [(⊕-∘
    (natural_w1 natural_s1)
    (natural_w2 natural_s2))
   (,(+ (term natural_w1) (term natural_w2) 1)
    ,(+ (term natural_s1) (term natural_s2) 1))])

(define-metafunction SP-Graph
  ⊕-∥ : $ $ -> $
  [(⊕-∥
    (natural_w1 natural_s1)
    (natural_w2 natural_s2))
   (,(+ (term natural_w1) (term natural_w2) 1)
    ,(+ (max (term natural_s1) (term natural_s2)) 1))])

(define-metafunction SP-Graph
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

; Model of a Work-Stealing scheduler
; ----------------------------------

(define-extended-language WS-Scheduler
  SP-Graph

  (WG ::= (W ...))                  ; work groups
  (W ::=                            ; workers:
     B                              ;   busy
     S                              ;   intention to steal from worker w▹
     Z)                             ;   sleeping
  (B ::= (E e))                     ; busy workers

  (e ::= $ g)                       ; expressions
  (E ::=                            ; evaluation contexts:
     (▽ j▹)                         ;   to join at join point j▹
     (∘ E g)                        ;   to evaluate the second branch of a serial composition
     (∘ $ E)                        ;   to yield the result of a serial composition
     (∥ E g)                        ;   to evaluate the second branch of a parallel composition
     (∥ $ E))                       ;   to yield the result of a parallel composition

  (w▹ ::= natural)                  ; worker identifiers (index of a worker in W)

  (JS ::= ((j▹ J) ...))             ; join stores
  (j▹ ::=                           ; join identifiers (w▹1 w▹2): inv. w▹1 < w▹2
      (w▹                           ;   first worker (victim)
       w▹))                         ;   second worker (thief)
  (J ::=                            ; join contexts:
     (∥0 E)                         ;   to join with at least one worker (neither results ready)
     (∥1 E $)                       ;   to join with the first worker (exactly one result, namely $, is ready)
     (∥2 $ E)                       ;   to join with the second worker (exactly one result, namely $, is ready)
     ∥I                             ;   to indicate a non-terminal machine state
     (∥F $))                        ;   to terminate the machine, yielding final result $

  (△I ::= ((w▹ ...) ...)))          ; steal intentions: for each worker, a fifo queue of ids of other workers waiting to steal
  
; Metafunctions
; -------------

(define-metafunction WS-Scheduler
  Mk-right-open-range : natural natural -> (natural ...)
  [(Mk-right-open-range natural_lo natural_hi)
   ,(range (term natural_lo) (term natural_hi))])

(define-metafunction WS-Scheduler
  Length : (any ...) -> natural
  [(Length (any_1 ...)) ,(length (term (any_1 ...)))])

(define-metafunction WS-Scheduler
  Set-subtract : (natural ...) (natural ...) -> (natural ...)
  [(Set-subtract (natural_a ...) (natural_b ...))
   ,(set->list (set-subtract (list->set (term (natural_a ...))) (list->set (term (natural_b ...)))))])

(define-metafunction WS-Scheduler
  Set-intersect : (natural ...) (natural ...) -> (natural ...)
  [(Set-intersect (natural_a ...) (natural_b ...))
   ,(set->list (set-intersect (list->set (term (natural_a ...))) (list->set (term (natural_b ...)))))])

(define (inject xs ivps)
  (foldl (λ (p xs)
           (match p
             [(list pos value) (list-set xs pos value)]))
         xs
         ivps))

(define (project xs is)
  (reverse
   (foldl (λ (pos ys)
            (cons (list pos (list-ref xs pos)) ys))
          '()
          is)))

; later: assert that all indices are distinct
(define-metafunction WS-Scheduler
  Inject : (any ...) ((natural any) ...) -> (any ...)
  [(Inject (any_v ...) ((natural_i any_i) ...))
   ,(inject (term (any_v ...)) (term ((natural_i any_i) ...)))])

; later: assert that all indices are distinct
(define-metafunction WS-Scheduler
  Project : (any ...) (natural ...) -> ((natural any) ...)
  [(Project (any_v ...) (natural_i ...))
   ,(project (term (any_v ...)) (term (natural_i ...)))])

(define-metafunction WS-Scheduler
  Sparsify : (any ...) -> ((w▹ any) ...)
  [(Sparsify (any_1 ...))
   ,(map list (range (length (term (any_1 ...)))) (term (any_1 ...)))])

(define-metafunction WS-Scheduler
  Try-to-split-E : E w▹ w▹ -> (E E g) or #f
  [(Try-to-split-E (∘ E g) w▹_v w▹_t)
   (E_c (∘ E_1 g) g_2)
   (where (E_c E_1 g_2) (Try-to-split-E E w▹_v w▹_t))]
  [(Try-to-split-E (∘ $ E) w▹_v w▹_t)
   (E_c (∘ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E w▹_v w▹_t))]
  [(Try-to-split-E (∥ E g) w▹_v w▹_t)
   (E (▽ (w▹_v w▹_t)) g)]
  [(Try-to-split-E (∥ $ E) w▹_v w▹_t)
   (E_c (∥ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E w▹_v w▹_t))]
  [(Try-to-split-E _ _ _) #f])

(define-metafunction WS-Scheduler
  Handle-△ : ((w▹ W) (w▹ W)) (((w▹ W) (w▹ W)) ... JS) -> (((w▹ W) (w▹ W)) ... JS)
  [(Handle-△ ((w▹_v (E_v e_v)) (w▹_t S)) (((w▹_va W_va) (w▹_ta W_ta)) ... ((j▹_a J_a) ...)))
   (((w▹_v (E_v2 e_v)) (w▹_t ((▽ j▹_1) g_t))) (((w▹_va W_va) (w▹_ta W_ta)) ... ((j▹_1 J_1) (j▹_a J_a) ...)))
   (where (E_c E_v2 g_t) (Try-to-split-E E_v w▹_v w▹_t))
   (where (j▹_1 J_1) ((w▹_v w▹_t) (∥0 E_c)))]
  [(Handle-△ _ (((w▹_va W_va) (w▹_ta W_ta)) ... JS))
   (((w▹_va W_va) (w▹_ta W_ta)) ... JS)]
  [(Handle-△ () JS)
   (JS)])

(define-metafunction WS-Scheduler
  Handle-△s : (((w▹ W) (w▹ W)) ...) JS -> (((w▹ W) (w▹ W)) ... JS)
  [(Handle-△s (((w▹_v W_v) (w▹_t W_t)) ...) JS)
  ,(foldl (λ (sa res) (term (Handle-△ ,sa ,res)))
          (term (JS))
          (term (((w▹_v W_v) (w▹_t W_t)) ...)))])

(define (hash a)
  (let* ([a (+ (+ a #x7ed55d16) (arithmetic-shift a 12))]
         [a (bitwise-xor (bitwise-xor a #xc761c23c) (arithmetic-shift a -19))]
         [a (+ (+ a #x165667b1) (arithmetic-shift a 5))]
         [a (bitwise-xor (+ a #xd3a2646c) (arithmetic-shift a 9))]
         [a (+ (+ a #xfd7046c5) (arithmetic-shift a 3))]
         [a (bitwise-xor (bitwise-xor a #xb55a4f09) (arithmetic-shift a -16))])
    (remainder a (arithmetic-shift 1 32))))

(define (random-in-range upper rs)
    (let* ([rs (hash rs)]
           [r (remainder rs upper)])
      (cons r rs)))

(define (random-list n rs)
  (letrec ([f (λ (n rs xs)
                (if (= n 0)
                    (cons (reverse xs) rs)
                    (f (sub1 n) (hash rs) (cons rs xs))))])
        (f n rs '())))

(define (steal-intentions rs stealers targets)
  (let ([nb-targets (length targets)])
    (if (= nb-targets 0)
        (cons '() rs)
        (match-let*
            ([nb-stealers (length stealers)]
             [(cons rvs rs) (random-list nb-stealers rs)]
             [target-idxs (map (λ (x) (remainder x nb-targets)) rvs)]
             [intentions (foldl (λ (stealer target-idx res)
                                  (list-update res target-idx (λ (ts) (append ts (list stealer)))))
                                (map list targets)
                                stealers
                                target-idxs)])
          (cons intentions rs)))))

(define-metafunction WS-Scheduler
  Mk-all-w▹ : WG -> (w▹ ...)
  [(Mk-all-w▹ WG) (Mk-right-open-range 0 (Length WG))])

(define-metafunction WS-Scheduler
  Is-stealing? : (w▹ W) -> boolean
  [(Is-stealing? (_ S)) #t]
  [(Is-stealing? _) #f])

(define-metafunction WS-Scheduler
  Filter-△-ready : WG △I -> (w▹ ...)
  [(Filter-△-ready WG △I)
   (Set-subtract (w▹ ...) ,(flatten (term △I)))
   (where ((w▹ W) ...) ,(filter (λ (x) (term (Is-stealing? ,x))) (term (Sparsify WG))))])

(define-metafunction WS-Scheduler
  Random-victims : (w▹ ...) (w▹ ...) natural -> (((w▹ w▹) ...) natural)
  [(Random-victims (w▹_t ...) (w▹_potential ...) natural_rs1)
   (((w▹_v w▹_t) ...) natural_rs2)
   (where natural_thieves (Length (w▹_t ...)))
   (where natural_potentials (Length (w▹_potential ...)))
   (where ((natural_target ...) natural_rs2)
          ,(match-let*
               ([(cons randoms rs2) (random-list (term natural_thieves) (term natural_rs1))]
                [target-idxs (map (λ (x) (remainder x (term natural_potentials))) randoms)])
             (list target-idxs rs2)))
   (where (w▹_v ...) ,(map (λ (pos) (list-ref (term (w▹_potential ...)) pos)) (term (natural_target ...))))])
             
(define-metafunction WS-Scheduler
  Next-△I : WG △I natural -> (△I natural)
  [(Next-△I WG △I_1 natural_rs1)
   (△I_2 natural_rs2)
   (where (((w▹_v w▹_t) ...) natural_rs2)
          (Random-victims (Filter-△-ready WG △I_1) (Mk-all-w▹ WG) natural_rs1))
   (where ((w▹_v (w▹_tq1 ...)) ...) (Project △I_1 (w▹_v ...)))
   (where ((w▹_tq2 ...) ...)
          ,(map (λ (xs x) (append xs (list x))) (term ((w▹_tq1 ...) ...)) (term (w▹_t ...))))
   (where △I_2 (Inject △I_1 ((w▹_v (w▹_tq2 ...)) ...)))])

; (term (Random-victims (Filter-△-ready (S S S S Z) (() () () () ())) (Mk-all-w▹ (S S S S Z)) 1122))
; (term (Next-△I (S S S S Z) (() () () () ()) 1122))
; todo: fix above metafunction. it needs to use something other than inject, because two or more steals to the same
; victim need to all end up in the victim queue. also, need to prevent attempt to steal from self.

(define-metafunction WS-Scheduler
  Make-steal-attempt : (w▹ (w▹ ...)) -> ((w▹ w▹) w▹ ...) or #f
  [(Make-steal-attempt (w▹_v (w▹_t w▹_ta ...)))
   ((w▹_v w▹_t) w▹_ta ...)]
  [(Make-steal-attempt _)
   #f])

(define-metafunction WS-Scheduler
  Make-steal-attempts : △I -> (((w▹ w▹) ...) △I)
  [(Make-steal-attempts △I)
   (((w▹_t w▹_v) ...) △I_2)
   (where ((w▹_v1 (w▹_t1 ...)) ...) (Sparsify △I))
   (where (((w▹_t w▹_v) w▹_va ...) ...) ,(filter (λ (x) x) (map (λ (p) (term (Make-steal-attempt ,p))) (term ((w▹_v1 (w▹_t1 ...)) ...)))))
   (where △I_2 (Inject △I ((w▹_t (w▹_va ...)) ...)))])

(define-metafunction WS-Scheduler
  Has-final-result? : JS -> $ or #f
  [(Has-final-result? ((j▹_b J_b) ... (_ (∥F $)) (j▹_a J_a) ...)) $]
  [(Has-final-result? _) #f])

; Reduction relation
; ------------------

(define-judgment-form WS-Scheduler
  #:mode (→B I O)
  #:contract (→B B B)

  [------------------- "•-$"
   (→B (E •) (E (1 1)))]
  
  [------------------------------------- "↑-∘"
   (→B ((∘ $_1 E) $_2) (E (⊕-∘ $_1 $_2)))]

  [------------------------------------- "↑-∥"
   (→B ((∥ $_1 E) $_2) (E (⊕-∥ $_1 $_2)))]

  [----------------------------------- "→-∘"
   (→B ((∘ E g_2) $_1) ((∘ $_1 E) g_2))]

  [----------------------------------- "→-∥"
   (→B ((∥ E g_2) $_1) ((∥ $_1 E) g_2))]

  [----------------------------------- "↓-∘"
   (→B (E (∘ g_1 g_2)) ((∘ E g_2) g_1))]

  [----------------------------------- "↓-∥"
   (→B (E (∥ g_1 g_2)) ((∥ E g_2) g_1))])

(define-judgment-form WS-Scheduler
  #:mode (→ I O)
  #:contract (→ W W)

  [(→B B_1 B_2)
   ----------- "B↦"
   (→ B_1 B_2)]

  [------- "S"
   (→ S S)]

  [------- "Z"
   (→ Z Z)])

(define-judgment-form WS-Scheduler
  #:mode (→▽ I I O O)
  #:contract (→▽ (w▹ W) JS (w▹ W) JS)

  [(where ((j▹_b J_b) ... (j▹ ∥I) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹ (∥F $)) (j▹_a J_a) ...))
   -------------------------------------------------------- "▽F"
   (→▽ (w▹_1 ((▽ j▹) $)) JS_1 (w▹_1 S) JS_2)]
  
  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥0 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹ (∥1 E $)) (j▹_a J_a) ...))
   --------------------------------------------------------- "▽I1"
   (→▽ (w▹_1 ((▽ j▹) $)) JS_1 (w▹_1 S) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥0 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹ (∥2 $ E)) (j▹_a J_a) ...))
   ---------------------------------------------------------- "▽I2"
   (→▽ (w▹_2 ((▽ j▹) $)) JS_1 (w▹_2 S) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥1 E $_2)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹_a J_a) ...))
   ---------------------------------------------------------- "▽E1"
   (→▽ (w▹_1 ((▽ j▹) $_1)) JS_1 (w▹_1 (E (⊕-∥ $_1 $_2))) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥2 $_1 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹_a J_a) ...))
   ----------------------------------------------------------- "▽E2"
   (→▽ (w▹_2 ((▽ j▹) $_2)) JS_1 (w▹_2 (E (⊕-∥ $_1 $_2))) JS_2)])

(define-judgment-form WS-Scheduler
  #:mode (⇒WJ I I O O)
  #:contract (⇒WJ ((w▹ W) ...) JS ((w▹ W) ...) JS)

  [------------------ "E"
   (⇒WJ () JS () JS)]
  
  [(→ W_1 W_2)
   (⇒WJ ((w▹_a1 W_a1) ...) JS_1 ((w▹_a2 W_a2) ...) JS_2)
   ------------------------------------------------------------------------ "W"
   (⇒WJ ((w▹ W_1) (w▹_a1 W_a1) ...) JS_1 ((w▹ W_2) (w▹_a2 W_a2) ...) JS_2)]

  
  [(→▽ (w▹ W_1) JS_1 (w▹ W_2) JS_2)
   (⇒WJ ((w▹_a1 W_a1) ...) JS_2 ((w▹_a2 W_a2) ...) JS_3)
   ------------------------------------------------------------------------ "J"
   (⇒WJ ((w▹ W_1) (w▹_a1 W_a1) ...) JS_1 ((w▹ W_2) (w▹_a2 W_a2) ...) JS_3)])

(define-judgment-form WS-Scheduler
  #:mode (⇒ I I I I O O O O)
  #:contract (⇒ WG JS △I natural WG JS △I natural)

  ; later: introduce rule to handle terminal state

  [(side-condition ,(not (term (Has-final-result? JS_1))))
   (where (((w▹_v? w▹_t?) ...) △I_2) (Make-steal-attempts △I_1))
   (where ((w▹_v? W_v?) ...) (Project (W_1 ...) (w▹_v? ...)))
   (where ((w▹_t? W_t?) ...) (Project (W_1 ...) (w▹_t? ...)))
   (where (((w▹_v* W_v*) (w▹_t* W_t*)) ... JS_2)
          (Handle-△s (((w▹_v? W_v?) (w▹_t? W_t?)) ...) JS_1))
   (where (W_2 ...) (Inject (W_1 ...) ((w▹_v* W_v*) ... (w▹_t* W_t*) ...)))
   (where (w▹_all ...) (Mk-all-w▹ (W_1 ...)))
   (where (w▹_step ...) (Set-subtract (w▹_all ...) (w▹_v* ... w▹_t* ...)))
   (⇒WJ (Project (W_1 ...) (w▹_step ...)) JS_2 ((w▹_r2 W_r2) ...) JS_3)
   (where (W_3 ...) (Inject (W_2 ...) ((w▹_r2 W_r2) ...)))
   (where (△I_3 natural_rs2) (Next-△I (W_3 ...) △I_2 natural_rs1))
   ------------------------------------------------------------------- "Step"
   (⇒ (W_1 ...) JS_1 △I_1 natural_rs1 (W_3 ...) JS_3 △I_3 natural_rs2)])

; Unit tests
; ----------

(define E1
  (term (∘ (∥ (∘ (▽ (123 456)) (∘ • •)) (∥ • •)) •)))

(define g1
  (term
   (∥ (∘ • •) (∘ • •))))

;(build═derivations (⟶ ((,E1 •)) () (()) 0 WG JS SI natural))
