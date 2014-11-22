(ns genetic-algorithm.core-test
  (:require [clojure.test :refer :all]
            [genetic-algorithm.core :refer :all]))

(deftest test-random-bits
  (testing "random-bits"
    (is (every? #{0 1} (take 10 (random-bits))))))

(deftest test-new-bit-string-chromosome
  (testing "new-bit-string-chromosome"
    (let [c (take 10 (random-bits))]
      (and (is (every? #{0 1} (take 10 (random-bits))))
           (is (= 10 (count c)))))))

(deftest test-new-population
  (testing "new-population"
    (let [pop (take 2 (new-population (fn [] '(1 1 1)) count-ones-fitness-func))]
      (and
        (is (= 2 (count pop)))
        (is (= 3 (count (:chr (first pop)))))
        (is (= 3 (:fitness (first pop))))))))

(deftest test-greater-fitness
  (testing "greater-fitness?"
    (and (is (greater-fitness? {:fitness 4} {:fitness 3}))
         (is (not (greater-fitness? {:fitness 3} {:fitness 4}))))))

(deftest test-sort-pop
  (testing "sort-pop"
    (let [sorted-pop (sort-pop '({:fitness 2} {:fitness 3} {:fitness 1}))]
      (is (= 3 (:fitness (nth sorted-pop 0))))
      (is (= 2 (:fitness (nth sorted-pop 1))))
      (is (= 1 (:fitness (nth sorted-pop 2)))))))

(deftest test-add-fitness
  (testing "add-fitness"
    (is (= 3 (add-fitness 1 {:fitness 2})))))

(deftest test-sum-fitnesses
  (testing "sum-fitnesses"
    (is (= 6 (sum-fitnesses '({:fitness 1} {:fitness 2} {:fitness 3}))))))

;; TODO spin the wheel many times and ensure roughly 60% calling rate
(deftest test-spin-roulette-wheel
  (testing "spin-roulette-wheel"
    ; 60/40 split on probability
    (let [test-pop (sort-pop '({:chr '(1 1 1 1 1 1) :fitness 6}
                               {:chr '(1 1 1 1 0 0) :fitness 4}))]
      (print (spin-roulette-wheel 10 test-pop)))))

(deftest test-flip-bit
  (testing "flip-bit"
    (is (= 1 (flip-bit 0))
    (is (= 0 (flip-bit 1))))))

(clojure.test/run-tests)

