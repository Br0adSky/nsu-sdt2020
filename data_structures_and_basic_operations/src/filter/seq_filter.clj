(ns filter.seq-filter
  (:require [filter.filter :as default-filter]))


(defn to-feature [batch-count pred seq]
  (lazy-seq (cons (map #(default-filter/future-filter pred %) (take batch-count seq))
                  (to-feature batch-count pred (drop batch-count seq)))))

(defn to-chunks [seq task-count]
  (lazy-seq (let [splitted (split-at task-count seq)]
              (cons (first splitted) (to-chunks (second splitted) task-count)))))

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