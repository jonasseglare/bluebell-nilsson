(ns bluebell.nilsson.core-test
  (:require [symbol-analyzer.core :refer [analyze-sexp]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
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
  (is (spec/valid? ::nilsson/nilsson-let '[[:a a 9 :b :c b 30] a b c]))
  (is (not (spec/valid? ::nilsson/nilsson-let '[:a a 9 :b :c b 30 a b c]))))
