(ns integral.core)
(defn one-trapeze [start finish step f acc]
  (+ acc (* step (/ (+ (f start) (f finish)) 2))))

(defn count-integral [acc current step f integral-func]
  (integral-func (- current step) current step f acc))
(def my-func
  (fn [x]
    (Thread/sleep 1)
    (* x 2)))

(defn integral [f step]
  (let [memo-trapeze (memoize one-trapeze)]
    (fn [finish] (reduce (fn [acc a] (count-integral acc a step f memo-trapeze))
                         (range 0.0 (+ finish step) step)))))

