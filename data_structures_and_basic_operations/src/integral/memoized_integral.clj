(ns integral.memoized-integral
  (:require [integral.core :refer :all]))

(defn lazy-integral
  "Создание бесконечной последовательности, где элементы это рассчитанные на каждом шаге интегралы"
  [current-value f step acc]
  (lazy-seq (let [current-integral (+ acc (one-trapeze current-value (+ current-value step) step f))]
              (cons current-integral (lazy-integral (+ current-value step) f step current-integral)))))

(defn seq-integral [f step]
  "Инициализация последовательности"
    (lazy-integral 0 f step 0))

(defn calc-integral-to
  "Возвращает функцию позволяющую расчитать функцию до указанной точки"
  [f step]
  (let [seq (seq-integral f step)]
    (fn [x] (nth seq (dec (/ x step))))))