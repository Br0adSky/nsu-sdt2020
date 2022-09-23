(ns common.utils)

(defn my-contains?
  [coll val]
  (some? (some #(= val %) coll)))

(defn contains-all?
  [coll1 coll2]
  (reduce (fn [result current-val]
            (if (and result (my-contains? coll1 current-val))
              true
              false))
          coll2))