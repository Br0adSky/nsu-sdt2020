(ns data_structures.core_test
  (:require [clojure.test :refer :all]
            [common.utils :refer :all]
            [data_structures.words :as one_onelab]
            [data_structures.words2 :as one_twolab]))

(deftest testlab1_1

  (testing "Testing lab1_1"
    (is (my-contains? (one_onelab/iterate-words 2) '(:b :c)))
    (is (my-contains? (one_twolab/iterate-words 2) '(:b :c)))
    (is (contains-all? (one_onelab/iterate-words 2) (one_twolab/iterate-words 2)))))