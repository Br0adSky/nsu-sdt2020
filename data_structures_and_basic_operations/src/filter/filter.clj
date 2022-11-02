(ns filter.filter)
(def future-count
  4)

(defn my-filter [pred coll]
  (reduce (fn [acc i]
            (if (pred i)
              (conj acc i)
              acc))
          (vec '())
          coll))

(defn future-filter [list pred]
  (future (my-filter pred list)))

(defn size-of-colls [n]
  (int (/ n future-count)))

(defn get-colls [coll]
  (let [coll-size (count coll)
        part (size-of-colls coll-size)
        acc-start (vec '())]
    (if (< coll-size future-count)
      (conj acc-start coll)
      (loop [current-state coll
             acc acc-start]
        (if (= (dec future-count) (count acc))
          (conj acc current-state)
          (recur (drop part current-state)
                 (conj acc (take part current-state))))))))

(defn long-fun [n]
  (Thread/sleep 10)
  (even? n))

(defn main [items pred]
  (->> (get-colls items)
       (map #(future-filter % pred))
       (doall)
       (map deref)
       (reduce concat)
       (doall)))
