(ns filter.filter)

(defn future-filter [pred list]
  (future (doall (filter pred list))))

(defn dec-or-zero [n]
  (if (zero? n)
    n
    (dec n)))

(defn get-colls
  ([coll future-count]
   (let [coll-size (count coll)
         quot (quot coll-size future-count)
         rem (rem coll-size future-count)]
     (get-colls coll quot rem)))

  ([coll quot rem]
  (lazy-seq
    (when (not-empty coll)
      (if (zero? rem)
        (cons (take quot coll) (get-colls (nthrest coll quot) quot rem))
        (cons (take (inc quot) coll) (get-colls (nthrest coll (inc quot)) quot (dec-or-zero rem))))))))


(defn main [items pred future-count]
  (->> (get-colls items future-count)
       (map #(future-filter % pred))
       (doall)
       (map deref)
       (apply concat)
       (doall)))
