(ns bluebell.nilsson.core
  (:require [symbol-analyzer.core :as analyzer]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as spec]
            [symbol-analyzer.extraction :as extraction]))

(defn except
  "Mark a symbol as not being tracked"
  [x]
  x)

(defmacro either
  "Choose the first symbol that is not nil"
  [& symbols]
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
                        :req-all  ; Require all symbols to be non-nil
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

(defn generate-binding [[symbol-set bindings] bd]
  (let [dirs (set (:directives bd))]
    [symbol-set (reduce into bindings [[(:symbols bd) (:expr bd)]])]))

(defn generate-bindings [bindings]
  (let [bd (reduce generate-binding [#{} []] bindings)]
    (println "bd=" bd)
    (second bd)
    []))

(defn generate-nlet [parsed]
  `(let ~(generate-bindings
          (:bindings parsed))
     ~@(:body parsed)))

(defmacro nlet [& args]
  (let [parsed (spec/conform ::nilsson-let args)]
    (if (= parsed ::spec/invalid)
      (throw (ex-info "Failed to parse nlet"
                      {:explanation (spec/explain-str ::nilsson-let args)}))
      (generate-nlet parsed))))


(comment
  (do

    (macroexpand '(nlet [:req-one k (if (= :a :a) 3)] k))

    ))

#_(defn known-symbols [expr]
  (->> expr
       vals
       ))
