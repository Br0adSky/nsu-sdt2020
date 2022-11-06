(ns filter.seq-filter
  (:require [filter.filter :as default-filter]))

(defn future-filter [pred seq]
  (future (default-filter/my-filter pred seq)))

(defn to-feature [batch-count pred seq]
  (lazy-seq (cons (map #(future-filter pred %) (take batch-count seq))
                  (to-feature batch-count pred (drop batch-count seq)))))

(defn to-chunks [seq task-count]
  (lazy-seq (cons (take task-count seq)
                  (to-chunks (drop task-count seq)
                             task-count))))

(defn filter-pred [n]
  (Thread/sleep 10)
  (even? n))

(defn main1 [seq task-count batch-count pred]
  (->>
    ;seq: (chunk1) (chunk2) (chunk3)...
    (to-chunks seq task-count)
    ;seq: (chunks1-3 batched to filtered feature) (chunks4-6 batched to feature) ...
    (to-feature batch-count pred)
    (map doall)
    ;seq: (chunks1-3 derefed from feature)...
    (map #(map deref %))
    (apply concat)
    (apply concat)
    ))
(defn my-concat [seq]
  (apply concat seq))