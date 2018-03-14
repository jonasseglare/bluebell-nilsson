(ns bluebell.nilsson.core
  (:require [symbol-analyzer.core :as analyzer]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as spec]
            [symbol-analyzer.extraction :as extraction]))

(defn except [x]
  x)

(defmacro either [& symbols]
  (assert (every? symbol? symbols))
  `(or ~@(map (fn [x] `(except ~x)) symbols)))

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

(defn disp [x]
  x)

(defn analyze-symbols [expr symbol-transducer]
  (transduce
   (comp (filter symbol?)
         symbol-transducer)
   conj
   []
   (-> expr
       analyzer/analyze-sexp
       deep-flatten
       disp)))

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






#_(defn known-symbols [expr]
  (->> expr
       vals
       ))
