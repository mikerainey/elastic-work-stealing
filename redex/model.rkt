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
     (▽ j)                          ;   to join at join point associated with j
     (∘ E g)                        ;   to evaluate the second branch of a serial composition
     (∘ $ E)                        ;   to yield the result of a serial composition
     (∥ E g)                        ;   to evaluate the second branch of a parallel composition
     (∥ $ E))                       ;   to yield the result of a parallel composition

  (JS ::= (J ...))                  ; join stores
  (J ::=                            ; join contexts:
     (∥0 j j E)                     ;   to join with victim and thief, with continuation E
     (∥v j $ E)                     ;   to join with the victim j, with thief result $, and continuation E
     (∥t $ j E)                     ;   to join with the thief, with victim result $, and continuation E
     (∥I j)                         ;   to indicate a non-terminal machine state
     (∥F $))                        ;   to terminate the machine, yielding final result $
  (j ::=                            ; join identifiers
     variable-not-otherwise-mentioned)

  (△I ::= ((w▹ ...) ...))           ; steal intentions: for each worker, a fifo queue of ids of other workers waiting to steal
  (w▹ ::= natural))                 ; worker identifiers (index of a worker in W)
  
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
  Try-to-split-E : E j -> (E E g) or #f
  [(Try-to-split-E (∘ E g) j_v)
   (E_c (∘ E_1 g) g_2)
   (where (E_c E_1 g_2) (Try-to-split-E E j_v))]
  [(Try-to-split-E (∘ $ E) j_v)
   (E_c (∘ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E j_v))]
  [(Try-to-split-E (∥ E g) j_v)
   (E (▽ j_v) g)]
  [(Try-to-split-E (∥ $ E) j_v)
   (E_c (∥ $ E_1) g)
   (where (E_c E_1 g) (Try-to-split-E E j_v))]
  [(Try-to-split-E _ _) #f])

(define-metafunction WS-Scheduler
  Handle-△ : ((w▹ W) (w▹ W)) (((w▹ W) (w▹ W)) ... JS) -> (((w▹ W) (w▹ W)) ... JS)
  [(Handle-△ ((w▹_v (E_v e_v)) (w▹_t S)) (((w▹_va W_va) (w▹_ta W_ta)) ... (J_a ...)))
   (((w▹_v (E_v2 e_v)) (w▹_t ((▽ j_t) g_t))) ((w▹_va W_va) (w▹_ta W_ta)) ... (J_1 J_a ...))
   (where (j_v j_t) ,(variables-not-in (term (J_a ...)) (term (jv jt))))
   (where (E_c E_v2 g_t) (Try-to-split-E E_v j_v))
   (where J_1 (∥0 j_v j_t E_c))]
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
  [(Random-victims (w▹_t ...) () natural_rs)
   (() natural_rs)]
  [(Random-victims (w▹_t ...) (w▹_potential ...) natural_rs1)
   (((w▹_v w▹_t2) ...) natural_rs2)
   (where natural_thieves (Length (w▹_t ...)))
   (where natural_potentials (Length (w▹_potential ...)))
   (where ((natural_target ...) natural_rs2)
          ,(match-let*
               ([(cons randoms rs2) (random-list (term natural_thieves) (term natural_rs1))]
                [target-idxs (map (λ (x) (remainder x (term natural_potentials))) randoms)])
             (list target-idxs rs2)))
   (where ((w▹_v w▹_t2) ...)
          ,(let ([potential-ixs (term (natural_target ...))]
                 [w▹-ts (term (w▹_t ...))]
                 [potentials (term (w▹_potential ...))]
                 [nb-potentials (term natural_potentials)])
             (filter (λ (p)
                       (match p
                         [(list w▹-v w▹-t)
                          (not (= w▹-v w▹-t))]))
                     (map (λ (w▹-t potential-ix)
                            (let* ([w▹-v (list-ref potentials potential-ix)]
                                   [w▹-v (if (= w▹-t w▹-v) ; to prevent a thief targeting itself
                                             (list-ref potentials (remainder (add1 potential-ix) nb-potentials))
                                             w▹-v)])
                              (term (,w▹-v ,w▹-t))))
                          w▹-ts potential-ixs))))])
             
(define-metafunction WS-Scheduler
  Next-△I : WG △I natural -> (△I natural)
  [(Next-△I WG △I_1 natural_rs1)
   (△I_2 natural_rs2)
   (where (((w▹_v w▹_t) ...) natural_rs2)
          (Random-victims (Filter-△-ready WG △I_1) (Mk-all-w▹ WG) natural_rs1))
   (where △I_2
          ,(foldl (λ (w▹-v w▹-t △I)
                    (list-set △I w▹-v (append (list-ref △I w▹-v) (list w▹-t))))
                  (term △I_1)
                  (term (w▹_v ...))
                  (term (w▹_t ...))))])

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
  Has-final-result? : JS -> boolean
  [(Has-final-result? (J_b ... (∥F $) J_a ...)) #t]
  [(Has-final-result? _) #f])

(define-metafunction WS-Scheduler
  Divide : (any ...) -> ((any ...) (any ...)) or #f
  [(Divide ()) #f]
  [(Divide (_)) #f]
  [(Divide (any_1 ... any_2 ...))
   ((any_1 ...) (any_2 ...))
   (side-condition (let ([n (length (term (any_1 ... any_2 ...)))])
                     (= (length (term (any_1 ...))) (quotient n 2))))])
   
; Reduction relation
; ------------------

(define-judgment-form WS-Scheduler
  #:mode (→ I O)
  #:contract (→ B B)

  [------------------- "•-$"
   (→ (E •) (E (1 1)))]
  
  [------------------------------------- "↑-∘"
   (→ ((∘ $_1 E) $_2) (E (⊕-∘ $_1 $_2)))]

  [------------------------------------- "↑-∥"
   (→ ((∥ $_1 E) $_2) (E (⊕-∥ $_1 $_2)))]

  [----------------------------------- "→-∘"
   (→ ((∘ E g_2) $_1) ((∘ $_1 E) g_2))]

  [----------------------------------- "→-∥"
   (→ ((∥ E g_2) $_1) ((∥ $_1 E) g_2))]

  [----------------------------------- "↓-∘"
   (→ (E (∘ g_1 g_2)) ((∘ E g_2) g_1))]

  [----------------------------------- "↓-∥"
   (→ (E (∥ g_1 g_2)) ((∥ E g_2) g_1))])

(define-judgment-form WS-Scheduler
  #:mode (⟶ I I O O)
  #:contract (⟶ ((w▹ W) ...) JS ((w▹ W) ...) JS)

  [------------------ "⟶:∅"
   (⟶ () JS () JS)]

  [------------------ "⟶:S"
   (⟶ ((w▹ S)) JS ((w▹ S)) JS)]

  [------------------ "⟶:Z"
   (⟶ ((w▹ Z)) JS ((w▹ Z)) JS)]

  [(→ B_1 B_2)
   ------------------ "⟶:→"
   (⟶ ((w▹ B_1)) JS ((w▹ B_2)) JS)]

  [(where (J_b ... (∥I j) J_a ...) JS_1)
   (where JS_2 (J_b ... (∥F $) J_a ...))
   -------------------------------------------- "⟶:▽F"
   (⟶ ((w▹_1 ((▽ j) $))) JS_1 ((w▹_1 S)) JS_2)]
  
  [(where (J_b ... (∥0 j_v j_t E) J_a ...) JS_1)
   (where JS_2 (J_b ... (∥v j_v $ E) J_a ...))
   --------------------------------------------- "⟶:▽-∥0-∥v"
   (⟶ ((w▹_t ((▽ j_t) $))) JS_1 ((w▹_t S)) JS_2)]

  [(where (J_b ... (∥0 j_v j_t E) J_a ...) JS_1)
   (where JS_2 (J_b ... (∥t $ j_t E) J_a ...))
   --------------------------------------------- "⟶:▽-∥0-∥t"
   (⟶ ((w▹_v ((▽ j_v) $))) JS_1 ((w▹_v S)) JS_2)]

  [(where (J_b ... (∥v j_v $_t E) J_a ...) JS_1)
   (where JS_2 (J_b ... J_a ...))
   --------------------------------------------------------------- "⟶:▽-∥v"
   (⟶ ((w▹_v ((▽ j_v) $_v))) JS_1 ((w▹_v (E (⊕-∥ $_v $_t)))) JS_2)]

  [(where (J_b ... (∥t $_v j_t E) J_a ...) JS_1)
   (where JS_2 (J_b ... J_a ...))
   --------------------------------------------------------------- "⟶:▽-∥t"
   (⟶ ((w▹_t ((▽ j_t) $_t))) JS_1 ((w▹_t (E (⊕-∥ $_v $_t)))) JS_2)]

  [(where (((w▹_1 W_1) ...) ((w▹_2 W_2) ...)) (Divide ((w▹ W) ...)))
   (⟶ ((w▹_1 W_1) ...) JS ((w▹_1a W_1a) ...) JS_1)
   (⟶ ((w▹_2 W_2) ...) JS_1 ((w▹_2a W_2a) ...) JS_2)
   ----------------------------------------------------------------- "⟶:÷"
   (⟶ ((w▹ W) ...) JS ((w▹_1a W_1a) ... (w▹_2a W_2a) ...) JS_2)])

(define-judgment-form WS-Scheduler
  #:mode (⇒★ I I I I O O O O)
  #:contract (⇒★ WG JS △I natural WG JS △I natural)

  [(side-condition (Has-final-result? JS))
   ----------------------------------------------- "⇒★:t"
   (⇒★ WG JS △I natural_rs WG JS △I natural_rs)]

  [(side-condition ,(not (term (Has-final-result? JS_1))))
   (where/error (((w▹_v? w▹_t?) ...) △I_2) (Make-steal-attempts △I_1))
   (where/error ((w▹_v? W_v?) ...) (Project (W_1 ...) (w▹_v? ...)))
   (where/error ((w▹_t? W_t?) ...) (Project (W_1 ...) (w▹_t? ...)))
   (where/error (((w▹_v* W_v*) (w▹_t* W_t*)) ... JS_2)
          (Handle-△s (((w▹_v? W_v?) (w▹_t? W_t?)) ...) JS_1))
   (where/error (W_2 ...) (Inject (W_1 ...) ((w▹_v* W_v*) ... (w▹_t* W_t*) ...)))
   (where/error (w▹_all ...) (Mk-all-w▹ (W_1 ...)))
   (where/error (w▹_step ...) (Set-subtract (w▹_all ...) (w▹_v* ... w▹_t* ...)))
   (⟶ (Project (W_1 ...) (w▹_step ...)) JS_2 ((w▹_r2 W_r2) ...) JS_3)
   (where/error (W_3 ...) (Inject (W_2 ...) ((w▹_r2 W_r2) ...)))
   (where/error (△I_3 natural_rs2) (Next-△I (W_3 ...) △I_2 natural_rs1))
   (⇒★ (W_3 ...) JS_3 △I_3 natural_rs2 (W_4 ...) JS_4 △I_4 natural_rs3)
   ------------------------------------------------------------------- "⇒★:s"
   (⇒★ (W_1 ...) JS_1 △I_1 natural_rs1 (W_4 ...) JS_4 △I_4 natural_rs3)])

(define-judgment-form WS-Scheduler
  #:mode (⇒ I I I O)
  #:contract (⇒ g natural natural $)

  [(where JS ((∥I j0)))
   (where (W ...) ,(make-list (sub1 (term natural_workers)) (term S)))
   (where WG (((▽ j0) g) W ...))
   (where △I ,(make-list (term natural_workers) (term ())))
   (⇒★ WG JS △I natural_rs _ JS_f _ _)
   (where (_ ... (∥F $) _ ...) JS_f)
   --------------------------------------------------------------------------- "⇒:i"
   (⇒ g natural_workers natural_rs $)])

; Unit tests
; ----------

(define (test-eval p n)
  (begin
    (for/list ([i (in-range n)])
      (let ([g (generate-term WS-Scheduler g 3)]
            [rs (hash i)])
        (begin
          (printf "~a~n" g)
        (test-equal (judgment-holds (⇒ ,g ,p ,rs $) $)
                    (term ((g→$ ,g)))))))
    '()))

(define E1
  (term (∘ (∥ (∘ (▽ j0) (∘ • •)) (∥ • •)) •)))

(define g1
  (term
   (∥ (∘ • •) (∘ • •))))

(define g2
  (term
   (∥ (∘ • •) (∥ • •))))

(define g4 (term (∘ (∥ (∥ • •) •) (∥ • (∥ • •)))))
(judgment-holds (⇒ ,g4 3 2313 $) $)

;(build═derivations (⟶ ((,E1 •)) () (()) 0 WG JS SI natural))
