(ns integral.core)

(defn one-trapeze [start finish step f]
  (* step (/ (+ (f start) (f finish)) 2)))

(defn my-func [x]
  (Thread/sleep 1)
  (* x 2))

(defn count-integral [to step f memoized-fun]
  (if (> to step)
    (+ (one-trapeze (- to step) to step f) (memoized-fun (- to step) step f memoized-fun))
    (one-trapeze 0 to step f)))


(defn integral [f step]
  (let [memo-integral (memoize count-integral)]
    (fn [finish]
     (memo-integral finish step f memo-integral))))

