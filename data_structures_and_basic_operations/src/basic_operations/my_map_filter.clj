(ns basic-operations.my_map_filter)

(defn my-map [function coll]
  (reduce (fn [acc i]
            (conj acc (function i)))
          (vec '())
          coll))

(defn my-filter [pred coll]
  (reduce (fn [acc i]
            (if (pred i)
              (conj acc i)
              acc))
          (vec '())
          coll))
