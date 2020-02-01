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
     (∥1 $ E)                       ;   to join with the first worker (exactly one result, namely $, is ready)
     (∥2 E $)                       ;   to join with the second worker (exactly one result, namely $, is ready)
     (∥F $))                        ;   to terminate the machine, yielding final result $

  (SI ::= ((w▹ ...) ...)))
  
; Metafunctions
; -------------
   
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
  Insert-JS : (j▹ J) JS -> JS
  [(Insert-JS (j▹_1 J_1) ((j▹_b2 J_b2) ... (j▹_1 _) (j▹_a2 J_a2) ...))
   ((j▹_b2 J_b2) ... (j▹_1 J_1) (j▹_a2 J_a2) ...)]
  [(Insert-JS (j▹_1 J_1) ((j▹_a2 J_a2) ...))
   ((j▹_1 J_1) (j▹_a2 J_a2) ...)])

(define-metafunction WS-Scheduler
  Handle-steal-attempt : (w▹ W) (w▹ W) JS -> ((w▹ W) (w▹ W) JS) or #f
  [(Handle-steal-attempt (w▹_v (E_v e_v)) (w▹_t S) JS)
   ((w▹_v (E_v2 e_v)) (w▹_t ((▽ j▹_1) g_t)) JS_2)
   (where (E_c E_v2 g_t) (Try-to-split-E E_v w▹_v w▹_t))
   (where (j▹_1 J_1) ((w▹_v w▹_t) (∥0 E_c)))
   (where JS_2 (Insert-JS (j▹_1 J_1) JS_1))]
  [(Handle-steal-attempt (w▹_v W_v) (w▹_t S) JS)
   ((w▹_v W_v) (w▹_t S) JS)]
  [(Handle-steal-attempt _ _ _)
   #f])

(define-metafunction WS-Scheduler
  Merge-JS : JS JS -> JS
  [(Merge-JS JS_1 JS_2)
   ,(foldl (λ (j JS2) (term (Insert-JS (term j) (term JS2)))) (term JS_2) (term JS_1))])

(define-metafunction WS-Scheduler
  Merge-JSs : (JS ...) JS -> JS
  [(Merge-JSs (JS_a ...) JS_b)
   ,(foldl (λ (JS1 JS0) (term (Merge-JS ,JS1 ,JS0))) (term JS_b) (term (JS_a ...)))])

(define E1
  (term (∘ (∥ (∘ (▽ (123 456)) (∘ • •)) (∥ • •)) •)))

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

(define (sparsify xs)
  (let ([n (length xs)])
    (map list (range n) xs)))

(define (unsparsify xs)
  (map cadr (sort xs < #:key car)))  

(define-metafunction WS-Scheduler
  Sparsify : (any ...) -> ((w▹ any) ...)
  [(Sparsify (any ...)) ,(sparsify (term (any ...)))])

(define-metafunction WS-Scheduler
  Unsparsify : ((w▹ any) ...) -> (any ...)
  [(Unsparsify ((w▹ any) ...)) ,(unsparsify (term ((w▹ any) ...)))])

(define (inject xs ivps)
  (foldl (λ (p xs)
           (match p
             [(list pos value) (list-set xs pos value)]))
         xs
         ivps))

(define (project xs is)
  (reverse
   (foldl (λ (pos ys)
            (cons (list-ref xs pos) ys))
          '()
          is)))

(define-metafunction WS-Scheduler
  Make-steal-attempt : (w▹ (w▹ ...)) -> ((w▹ w▹) w▹ ...) or #f
  [(Make-steal-attempt (w▹_v (w▹_t w▹_ta ...)))
   ((w▹_v w▹_t) w▹_ta ...)]
  [(Make-steal-attempt _)
   #f])

(define-metafunction WS-Scheduler
  Make-steal-attempts : SI -> (((w▹ w▹) ...) SI)
  [(Make-steal-attempts SI)
   (((w▹_t w▹_v) ...) SI_2)
   (where ((w▹_v1 (w▹_t1 ...)) ...) (Sparsify SI))
   (where (((w▹_t w▹_v) w▹_va ...) ...) ,(filter (λ (x) x) (map (λ (p) (term (Make-steal-attempt ,p))) (term ((w▹_v1 (w▹_t1 ...)) ...)))))
   (where SI_2 ,(inject (term SI) (term ((w▹_t (w▹_va ...)) ...))))])

(define-metafunction WS-Scheduler
  Has-final-result? : JS -> $ or #f
  [(Has-final-result? ((j▹_b J_b) ... (_ (∥F $)) (j▹_a J_a) ...)) $]
  [(Has-final-result? _) #f])

; Reduction relation
; ------------------

(define-judgment-form WS-Scheduler
  #:mode (↦ I O)
  #:contract (↦ B B)

  [------------------- "•-$"
   (↦ (E •) (E (1 1)))]
  
  [------------------- "↑-∘"
   (↦ ((∘ $_1 E) $_2) (E (⊕-∘ $_1 $_2)))]

  [------------------- "↑-∥"
   (↦ ((∥ $_1 E) $_2) (E (⊕-∥ $_1 $_2)))]

  [------------------- "→-∘"
   (↦ ((∘ E g_2) $_1) ((∘ $_1 E) g_2))]

  [------------------- "→-∥"
   (↦ ((∥ E g_2) $_1) ((∥ $_1 E) g_2))]

  [------------------- "↓-∘"
   (↦ (E (∘ g_1 g_2)) ((∘ E g_2) g_1))]

  [------------------- "↓-∥"
   (↦ (E (∥ g_1 g_2)) ((∥ E g_2) g_1))])

(define-judgment-form WS-Scheduler
  #:mode (→ I O)
  #:contract (→ W W)

  [(↦ B_1 B_2)
   ------------------- "B↦"
   (→ B_1 B_2)]

  [------------------- "S"
   (→ S S)]

  [------------------- "Z"
   (→ Z Z)])

(define-judgment-form WS-Scheduler
  #:mode (⇓ I I O O)
  #:contract (⇓ (w▹ W) JS (w▹ W) JS)

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥0 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹ (∥1 $ E)) (j▹_a J_a) ...))
   --------------------------------------------------------- "j1"
   (⇓ (w▹_1 ((▽ j▹) $)) JS_1 (w▹_1 S) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥0 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹ (∥2 $ E)) (j▹_a J_a) ...))
   ---------------------------------------------------------- "j2"
   (⇓ (w▹_2 ((▽ j▹) $)) JS_1 (w▹_2 S) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥1 $_2 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹_a J_a) ...))
   ---------------------------------------------------------- "jc1"
   (⇓ (w▹_1 ((▽ j▹) $_1)) JS_1 (w▹_1 (E (⊕-∥ $_1 $_2))) JS_2)]

  [(where (w▹_1 w▹_2) j▹)
   (where ((j▹_b J_b) ... (j▹ (∥2 $_1 E)) (j▹_a J_a) ...) JS_1)
   (where JS_2 ((j▹_b J_b) ... (j▹_a J_a) ...))
   ----------------------------------------------------------- "jc2"
   (⇓ (w▹_2 ((▽ j▹) $_2)) JS_1 (w▹_2 (E (⊕-∥ $_1 $_2))) JS_2)])

(define-judgment-form WS-Scheduler
  #:mode (⟶ I I I I O O O O)
  #:contract (⟶ WG JS SI natural WG JS SI natural)

  [(side-condition ,(not (term (Has-final-result? JS_1))))
   (→ W_1 W_2) ...
   (where (((w▹_v w▹_t) ...) SI_2) (Make-steal-attempts SI_1))
   (where ((w▹_v W_v) ...) ,(project (term (W_1 ...)) (term (w▹_v ...))))
   (where ((w▹_t W_t) ...) ,(project (term (W_1 ...)) (term (w▹_t ...))))
   (where (((w▹_sv W_sv) (w▹st W_st) JS_s) ...) ((Handle-steal-attempt (w▹_v W_v) (w▹_t W_t) JS_1) ...))
   (where JS_2 (Merge-JSs (JS_s ...) JS_1))
   (where (W_3 ...) ,(inject (term (W_2 ...)) (term ((w▹_sv W_sv) ... (w▹st W_st) ...))))
   ------------------------------------------------------------------- "Step"
   (⟶ (W_1 ...) JS_1 SI_1 natural_rs1 (W_3 ...) JS_2 SI_2 natural_rs1)])

;(build-derivations (⟶ ((,E1 •)) () (()) 0 WG JS SI natural))

; Unit tests
; ----------

(define g1
  (term
   (∥ (∘ • •) (∘ • •))))
