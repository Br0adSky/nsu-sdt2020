(ns filter.seq-filter)
(def future-count
  4)

(defn my-filter
  [pred f start]
  (lazy-seq (let [a (f start)]
              (if (pred a)
                (cons a (my-filter pred f a))
                (my-filter pred f a)))))

(defn future-filter [pred f start]
  (future (my-filter pred f start)))

(defn size-of-colls [n]
  (int (/ n future-count)))

(defn get-colls [end]
  (let [part (size-of-colls end)]
    (loop [acc1 part
           acc (list 0)]
      (if (= (- future-count 1) (count acc))
        (reverse (cons (- end acc1) acc))
        (recur (+ part acc1) (cons acc1 acc))))))

(defn filter-pred [n]
  (Thread/sleep 10)
  (even? n))
;можно попробовать каждое новое значение отфильтрованной последовательности складывать на отдельную ноду, но встает вопрос, как потом этим
;последовательностям делать deref и вообще совмещать... concat умеет склеивать lazy-seq?
(defn main [pred f start end]
  (->> (get-colls end)
       (map #(future-filter pred f %))
       (doall)
       (map deref)
       (reduce concat)
       (doall)))

(println (take 5 (my-filter even? inc 1)))
(println (take 5 (filter even? (iterate inc 1))))
(println (main even? inc 1 5))