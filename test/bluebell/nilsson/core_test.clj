(ns bluebell.nilsson.core-test
  (:require [symbol-analyzer.core :refer [analyze-sexp]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.string :as cljstr]
            [bluebell.nilsson.core :refer :all :as nilsson]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= #{'b 'c}
           (unknown-symbols '(let [a 9] (+ a b c)))))
    (is (= #{'a 'b 'c}
           (unknown-symbols '(do a (let [a 9] (+ a b c))))))
    (is (= #{'b}
           (unknown-symbols '(let [a 9] (+ a b (bluebell.nilsson.core/except c))))))
    (is (= #{'a 'b 'd 'e}
           (unknown-symbols-here (+ a b (except c) (d e)))))
    (is (= #{'k 'j}
           (unknown-symbols-here (do k (either a b c) (j)))))))

(deftest spec-test
  (is (spec/valid? ::nilsson/nilsson-let '[[a 9 b 30] a b c]))
  (is (spec/valid? ::nilsson/nilsson-let '[[:req-all a 9 :req-one b 30] a b c]))
  (is (not (spec/valid? ::nilsson/nilsson-let '[:req-all a 9 :req-one b 30 a b c]))))

(deftest find-all-symbols-test
  (is (= (find-all-symbols {:a [1 ['a 'b]]})
         '[a b]))
  (is (= (find-all-symbols {:a [1 ['a 'b {'c 'd}]]})
         '[a b c d])))

(defn safe-sqrt [x]
  (nlet
   [:req-one value (if (< 0 x) (Math/sqrt x))]
   value))

(defn neg-2-nil [x]
  (if (<= 0 x)
    x))

(defn my-positive-number [a b]
  (nlet [:req-one [x y] (map neg-2-nil [a b])]
        (or x y)))

(deftest safe-sqrt-test
  (is (= 2.0 (safe-sqrt 4.0)))
  (is (thrown? Throwable (safe-sqrt -4.0))))

(deftest my-pos-num-test
  (is (= 3 (my-positive-number 3 4)))
  (is (= 4 (my-positive-number -3 4)))
  (is (thrown? Throwable (my-positive-number -3 -4))))

(defn my-positive-numbers [a b]
  (nlet [:req-all [x y] (map neg-2-nil [a b])]
        [x y]))

(deftest my-pos-num-test-2
  (is (= [3 4] (my-positive-numbers 3 4)))
  (is (thrown? Throwable (my-positive-numbers -3 4))))

(defn normalize-vec-2 [[x y]]
  (nlet [sq-norm (+ (* x x) (* y y))
         norm0 (Math/sqrt sq-norm)
         norm (if (< 0 norm0) norm0)
         result [(/ x norm) (/ y norm)]]
        result))

(deftest norm-vec-test
  (is (nil? (normalize-vec-2 [0 0])))
  (is (nil? (normalize-vec-2 [0 0])))
  (is (= [(/ 3.0 5) (/ 4.0 5)] (normalize-vec-2 [3 4]))))

(defn parse-file [filename]
  (nlet [[file-contents file-exception] (result-or-exception (slurp filename))
         file-lines (cljstr/split-lines file-contents)]
        file-lines))
