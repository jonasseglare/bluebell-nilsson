(ns bluebell.nilsson.core-test
  (:require [symbol-analyzer.core :refer [analyze-sexp]]
            [clojure.test :refer :all]
            [bluebell.nilsson.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= #{'b 'c}
           (unknown-symbols '(let [a 9] (+ a b c)))))
    (is (= #{'a 'b 'c}
           (unknown-symbols '(do a (let [a 9] (+ a b c))))))
    (is (= #{'b}
           (unknown-symbols '(let [a 9] (+ a b (bluebell.nilsson.core/except c))))))
    (is (= #{'a 'b 'd 'e}
           (unknown-symbols-here (+ a b (except c) (d e)))))))
