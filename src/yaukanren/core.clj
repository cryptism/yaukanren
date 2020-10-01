(ns yaukanren.core)

;; Cribbing off of core.logic for naming conventions
(defrecord LVar [var])
(defn lvar [var] (LVar. var))
(defn lvar? [x] (instance? LVar x))
(defn lvar=? [x1 x2] (= (:var x1) (:var x2)))

(defn assp
  "Gets the first element matching the predicate in a list of pairs"
  [pred l]
  (first (filter #(pred (first %)) l)))

(defn walk
  "Walk the states in our vectors, returning a non-variable term or nil"
  [u s]
  (if-let [pr (and (lvar? u) (assp (partial lvar=? u) s))]
    (walk (second pr) s)
    u))

(def pair? #(and (vector? %1) (= (count %1) 2)))

(defn ext-s
  "Extend the substitution with a new binding"
  [x v s] (conj s [x v]))

(defn unify
  [u v s]
  (let [u (walk u s) v (walk v s)]
    (cond
      ; Looking at the same lvar
      (and (lvar? u) (lvar? v) (lvar=? u v)) s
      ; lhs is an lvar
      (lvar? u) (ext-s u v s)
      ; rhs is an lvar
      (lvar? v) (ext-s v u s)
      (and (pair? u) (pair? v)) (let [s (unify (first u) (first v) s)]
                                  (and s (unify (second u) (second v) s)))
      (= u v) s)))

(def mzero '())

(defn unit [sc] (cons sc mzero))

(defn === [u v]
  (fn [sc]
    (let [s (unify u v (first sc))]
      (if s (unit (list s (second sc))) mzero))))

(defn call-fresh
  "Updates f with a fresh variable binding"
  [f]
  (fn [sc]
    (let [c (second sc)]
      ((f (lvar c)) (list (first sc) (inc c))))))

(defn mplus [$1 $2]
  (cond
    (empty? $1) $2
    (fn? $1) (fn [] (mplus ($1) $2)) ; Immature stream case, would not halt otherwise
    :else (cons (first $1) (mplus (rest $1) $2))))

(defn mbind
  "Monad binding operation"
  [$ g]
  (cond
    (empty? $) mzero
    (fn? $) (fn [] (mbind ($) g)) ; Immature stream case, would not halt otherwise
    :else (mplus (g (first $)) (mbind (rest $) g))))

(defn ldisj [g1 g2]
  (fn [sc]
    (mplus (g1 sc) (g2 sc))))

(defn lconj [g1 g2]
  (fn [sc]
    (mbind (g1 sc) g2)))

;; Helper macros and whatnot

(defmacro zzz
  "Eta expansion helper"
  [g]
  `(fn [sc]
     (fn []
       (~g sc))))

(defmacro expandf [f f+ g1 g2 & args]
  `(~f (zzz ~g1) (~f+ ~g2 ~@args)))

(defmacro lconj+
  ([g] `(zzz ~g))
  ([g0 g1 & args]
   `(expandf lconj lconj+ ~g0 ~g1 ~@args)))

(defmacro ldisj+
  ([g] `(zzz ~g))
  ([g0 g1 & args]
   `(expandf ldisj ldisj+ ~g0 ~g1 ~@args)))

(defmacro conde
  "The disj+ of a non-empty sequence of conj+s"
  [g0 g1 & args]
  `(disj+ ~@`(conj+ ~g0 ~g1 ~@args)))

(defmacro fresh
  "Takes a list of variables and binds them to their goals, calling conj+ at the end"
  [xs & gs]
  (if (empty? xs)
    `(conj+ ~@gs)
    `(call-fresh
      (fn [x0]
        (fresh ~(seq (rest xs)) ~@gs)))))

(defn pull
  "Advances the stream until it's mature"
  [$]
  (if (fn? $) (pull ($)) $))

(defn take-all
  "Pulls all results from the stream"
  [$]
  (let [$ (pull $)]
    (if (empty? $) '() (cons (first $) (take-all (rest $))))))

(defn taken
  "Pulls n results from the stream"
  [n $]
  (if (zero? n) '()
      (let [$ (pull $)]
        (cond
          (empty? $) '()
          :else (cons (first $) (take (dec n) (rest $)))))))

;; Reification

(defn reify-name [n]
  (symbol (str "_" "." n)))

(defn reify-s
  "Reifies state"
  [v s]
  (let [v (walk v s)]
    (cond
      (lvar? v) (cons '(v (reify-name (count s))) s)
      (pair? v) (reify-s (second v) (reify-s (first v) s))
      :else s)))

(defn walk*
  "Walks over lvars, pairs or returns the object"
  [v s]
  (let [v (walk v s)]
    (cond
      (lvar? v) v
      (pair? v) '((walk* (first v) s) (walk* (second v) s))
      :else v)))

(defn reify-state-1st-var [sc]
  (let [v (walk* (LVar. 0) (first sc))]
    (walk* v (reify-s v '()))))

(defn mK-reify
  "Reifies the list of states `s-c*` by reifying each state's substitution
  wrt. the `n` first variables."
  [sc]
  (map reify-state-1st-var sc))

;; More stuff for miniKanrenisation

(def empty-state '('() 0))

(defn call-empty-state [g]
  (g empty-state))

(defmacro run
  [n xs & gs]
  `(mK-reify (take ~n (call-empty-state
                       (fresh ~xs
                              ~@gs)))))

(defmacro run*
  [xs & gs]
  `(mK-reify (take-all (call-empty-state
                        (fresh ~xs
                               ~@gs)))))
