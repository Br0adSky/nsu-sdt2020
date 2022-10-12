(ns integral.core)
(defn one-trapeze [start finish step f]
  (* step (/ (+ (f start) (f finish)) 2)))

(def memo-trapeze
  (memoize one-trapeze))

(defn count-integral [acc current step f integral-func]
  (+ acc (integral-func (- current step) current step f)))

(defn integral [finish f step]
  (reduce (fn [acc a] (count-integral acc a step f memo-trapeze))
          (range 0.0 (+ finish step) step)))

(def my-func
  (fn [x]
    (Thread/sleep 1)
    (* x 2)))


