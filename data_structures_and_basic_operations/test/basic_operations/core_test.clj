(ns basic-operations.core-test
  (:require [clojure.test :refer :all]
            [basic-operations.my_map_filter :as my_impl]
            [basic-operations.map_filter :as words_impl]
            [common.utils :refer :all]))

(deftest test_my_impl
  (testing "Testing map and filter"
    (is (empty? (my_impl/my-filter even? '(1 1 1 1 1))))
    (is (= (my_impl/my-map inc '(1 2 3)) '(2 3 4)))))

(deftest test_standart_words_impl
  (testing "Testing words impls using map/filter/iterate"
    (is (my-contains? (words_impl/iterate-words 2) '(:a :b)))
    (is (my-contains? (words_impl/iterate-words 1) '(:a)))))
