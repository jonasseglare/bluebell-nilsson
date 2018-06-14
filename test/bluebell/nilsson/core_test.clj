(ns bluebell.nilsson.core-test
  (:require [symbol-analyzer.core :refer [analyze-sexp]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.string :as cljstr]
            [clojure.java.io :as io]
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
  (is (spec/valid? ::nilsson/nilsson-let '[[:req a 9 :req-one b 30] a b c]))
  (is (not (spec/valid? ::nilsson/nilsson-let '[:req a 9 :req-one b 30 a b c]))))

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
  (nlet [:req [x y] (map neg-2-nil [a b])]
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

(spec/def ::file-data-spec (spec/* #{"a" "b" "c"}))

(defn parse-file [filename]
  (nlet [[file-contents file-exception] (result-or-exception (slurp filename))
         file-lines (cljstr/split-lines file-contents)
         [good-data bad-format-info] (conform-or-info ::file-data-spec file-lines)
         a-count (count (filter (partial = "a") good-data))
         bad-file  (do file-exception [:bad-file filename])
         bad-format [:bad-format bad-format-info]]
        (or a-count
            bad-file
            bad-format)))

(deftest parse-file-test
  (is (= 4 (parse-file (io/resource "goodfile.txt"))))
  (is (= :bad-format (first (parse-file (io/resource "badfile.txt")))))
  (is (= :bad-file (first (parse-file (io/resource "le.txt"))))))

(defn safe-sqrt-2 [x]
  (nlet [[good bad] (split-by-pred >= x 0)
         result (Math/sqrt good)
         error-msg [:bad-input bad]]
        (or result error-msg)))

(deftest safe-sqrt-2test
  (is (= 2.0 (safe-sqrt-2 4.0)))
  (is (= [:bad-input -3] (safe-sqrt-2 -3))))

(defn safe-sqrt-3 [x]
  (nlet [x-valid (pred >= x 0.0)
         result (Math/sqrt x-valid)]
        result))

(deftest safe-sqrt-3-test
  (is (= 2.0 (safe-sqrt-3 4.0)))
  (is (nil? (safe-sqrt-3 -4.0))))

(deftest nilor-test
  (is (= false (nilor false 4)))
  (is (= 4 (nilor nil 4))))

(defn bmi-calc [data-map]
  (nlet [height-m (:height-m data-map)
         mass-kg (:mass-kg data-map)

         height-in (:height-in data-map)
         mass-lb (:mass-lb data-map)

         bmi-si (/ mass-kg (* height-m height-m))
         bmi-en (* 703 (/ mass-lb (* height-in height-in)))]
        (nilor bmi-si bmi-en)))

(defn tagged? [tg x]
  (and (vector? x)
       (= tg (first x))))

(defn untag [tg x]
  (if (tagged? tg x)
    (second x)))

(defn untag-or-value [tg x]
  (apply-or-last-value untag tg x))




(defn unary-op [x]
  (nlet [x-sq (untag :square x)
         x-neg (untag :negate x)
         x-squared (* x-sq x-sq)
         x-negative (- x-neg)
         result (either x-squared x-negative)]
        result))

(deftest unary-op-test
  (is (= 9 (unary-op [:square 3])))
  (is (= -3 (unary-op [:negate 3]))))

(deftest bmi-calc-test
  (is (nil? (bmi-calc {:mass 119})))
  (is (number? (bmi-calc {:mass-kg 81 :height-m 1.73})))
  (is (number? (bmi-calc {:mass-lb 179 :height-in 68})))
  (is (nil? (bmi-calc {:mass-kg 81 :height-in 1.73}))))

(defn add-good-and-bad [g b]
  (nlet [good (untag :good g)
         bad (untag :bad b)
         result (+ good bad)]
        result))

(deftest agab
  (is (= 7 (add-good-and-bad [:good 3] [:bad 4])))
  (is (nil? (add-good-and-bad [:good 3] [:badd 4]))))
