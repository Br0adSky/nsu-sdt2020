(ns filter.filter)

(defn my-filter [pred coll]
  (reduce (fn [acc i]
            (if (pred i)
              (conj acc i)
              acc))
          (vec '())
          coll))

(defn future-filter [list pred]
  (future (my-filter pred list)))

(defn size-of-colls [n future-count]
  (int (/ n future-count)))

(defn get-colls [coll future-count]
  (let [coll-size (count coll)
        part (size-of-colls coll-size future-count)
        acc-start (vec '())]
    (if (< coll-size future-count)
      (conj acc-start coll)
      (loop [current-state coll
             acc acc-start]
        (if (= (dec future-count) (count acc))
          (conj acc current-state)
          (recur (drop part current-state)
                 (conj acc (take part current-state))))))))

(defn main [items pred future-count]
  (->> (get-colls items future-count)
       (map #(future-filter % pred))
       (doall)
       (map deref)
       (reduce concat)
       (doall)))
