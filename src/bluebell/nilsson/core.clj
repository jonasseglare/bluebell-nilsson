(ns bluebell.nilsson.core
  (:require [symbol-analyzer.core :as analyzer]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as spec]
            [symbol-analyzer.extraction :as extraction]
            [clojure.set :as cljset]))

(declare except)

(defn is-except? [x]
  (let [v (-> x meta :symbol-info :var)]
    (= (var except) v)))

(spec/def ::except (spec/cat :prefix is-except?
                              :value symbol?))

(spec/def ::subexpr (spec/or :except ::except
                             :coll coll?
                             :any any?))

(defn flatten? [x]
  (let [t (first (spec/conform ::subexpr x))]
    (= :coll t)))
(defn deep-flatten-into [dst x]
  (if (flatten? x)
    (reduce deep-flatten-into dst x)
    (conj dst x)))

(defn deep-flatten [x]
    (deep-flatten-into [] x))

(defn analyze-symbols [expr symbol-transducer]
  (transduce
   (comp (filter symbol?)
         symbol-transducer)
   conj
   []
   (-> expr
       walk/macroexpand-all
       analyzer/analyze-sexp
       deep-flatten)))

(defn unknown-symbols [expr]
  (set
   (analyze-symbols
    expr
    (comp  (map (fn [sym]
                  {:sym sym
                   :type (-> sym meta :symbol-info :type)}))
           (filter (fn [info]
                     (-> info
                         :type
                         (= :unknown))))
           (map :sym)))))

(defmacro unknown-symbols-here [expr]
  `(quote ~(unknown-symbols expr)))


(spec/def ::directive #{:req-one  ; Require one of the symbols to be non-nil
                        :req  ; Require all symbols to be non-nil
                        :exclude  ; Don't take the symbols into consideration
                        })

(spec/def ::symbols #(or (symbol? %)
                         (coll? %)))

(spec/def ::binding (spec/cat
                     :directives (spec/* ::directive)
                     :symbols ::symbols
                     :expr any?))

(spec/def ::bindings (spec/* ::binding))

(spec/def ::body (spec/* any?))

(spec/def ::nilsson-let (spec/cat :bindings (spec/spec ::bindings)
                                  :body ::body))

(defn find-all-symbols-sub [dst x]
  (cond
    (symbol? x) (conj dst x)
    (coll? x) (reduce find-all-symbols-sub dst x)
    :default dst))

(defn find-all-symbols [x]
  (find-all-symbols-sub [] x))

(defn req-all [symbols]
  `[_# (do ~@(map (fn [sym]
                    `(assert
                      (not (nil? ~sym))
                      ~(str "Symbol '" sym "' must not be nil.")))
                  symbols))])

(defn req-one [symbols]
  `[_# (assert (or ~@(map (fn [sym] `(not (nil? ~sym))) symbols))
              ~(str "At least one symbol in " symbols " must be non-nil."))])


(defn wrap-reqs [req-syms expr]
  (if (empty? req-syms)
    expr
    `(if (and ~@(map (fn [sym] `(not (nil? ~sym))) req-syms))
       ~expr)))

(defn generate-binding [[symbol-set bindings] bd]
  (let [dirs (set (:directives bd))
        all-symbols (find-all-symbols (:symbols bd))
        required-symbols (cljset/intersection symbol-set
                                              (unknown-symbols (:expr bd)))]
    [((if (contains? dirs :exclude) cljset/difference cljset/union)
      symbol-set
      (set all-symbols))
     (reduce into bindings [[(:symbols bd) (wrap-reqs required-symbols (:expr bd))]
                            (cond
                              (contains? dirs :req) (req-all all-symbols)
                              (contains? dirs :req-one) (req-one all-symbols)
                              :default [])])]))

(defn generate-bindings [bindings]
  (second (reduce generate-binding [#{} []] bindings)))

(defn generate-nlet [parsed]
  (let [the-let
        `(let ~(generate-bindings
                (:bindings parsed))
           ~@(:body parsed))]
    the-let))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro nlet [& args]
  (let [parsed (spec/conform ::nilsson-let args)]
    (if (= parsed ::spec/invalid)
      (throw (ex-info "Failed to parse nlet"
                      {:explanation (spec/explain-str ::nilsson-let args)}))
      (generate-nlet parsed))))

;; Evaluates


(defn except
  "Mark a symbol as not being tracked"
  [x]
  x)

(defmacro nilor [& args]
  (if (empty? args)
    nil
    `(let [f# ~(first args)]
       (if (nil? f#)
         (nilor ~@(rest args))
         f#))))

(defmacro either
  "Choose the first symbol that is not nil"
  [& symbols]
  (assert (every? symbol? symbols))
  `(nilor ~@(map (fn [x] `(except ~x)) symbols)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro result-or-exception [& body]
  `(try
     [(do ~@body) nil]
     (catch Throwable e#
       [nil e#])))

(defn conform-or-info [sp expr]
  (let [x (spec/conform sp expr)]
    (if (= x ::spec/invalid)
      [nil {:spec sp
            :expr expr}]
      [x nil])))

(defn apply-pred [pred x args]
  (apply pred (into [x] args)))

(defn split-by-pred [pred value & args]
  {:pre [(fn? pred)]}
  (if (apply-pred pred value args)
    [value nil]
    [nil value]))

(defn pred [f x & args]
  (if (apply-pred f x args)
    x))

(defn not-pred [f x & args]
  (if (not (apply-pred f x args))
    x))

(defn apply-or-last-value [f & args]
  (let [y (apply f args)]
    (if (nil? y)
      [nil (last args)]
      [y nil])))

(defn apply-or-first-value [f & args]
  (let [y (apply f args)]
    (if (nil? y)
      [nil (first args)]
      [y nil])))

(comment
  (do

    (macroexpand '(nlet [:req-one k (if (= :a :a) 3)] k))
    (macroexpand '(nlet [:req k (if (= :a :a) 3)] k))

    ))

#_(defn known-symbols [expr]
  (->> expr
       vals
       ))
