(ns integral.core_test
  (:require [clojure.test :as test]
            [integral.core :refer :all]
            [integral.memoized-integral :refer :all]))

(defn almost= [Expected Actual] (<= (Math/abs (double (- Actual Expected))) 1e-10))

(test/deftest main-test
  (test/testing "Testing integral"
    (test/is (almost= (my-func 100) 200))
    (test/is (almost= (integral 10 my-func 1) 100))
    (test/is (almost= (integral 5 my-func 1) 25))))

(let [get-integral (calc-integral-to my-func 1)]
  (test/deftest main-test-seq
    (test/testing "Testing seq integral"
      (test/is (almost= (get-integral 10) 100))
      (test/is (almost= (get-integral 5) 25)))))
