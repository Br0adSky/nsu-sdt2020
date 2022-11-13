(defn constant
  "Порождение константы"
  [num]
  (list ::const num))

(defn constant?
  "Проверка, является ли выражение константой"
  [expr]
  (= (first expr) ::const))

(defn constant-value
  "Получение значения константы"
  [const]
  (second const))


(defn variable
  "Порождение переменной"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Проверка, является ли выражение переменной"
  [expr]
  (= (first expr) ::var))

(defn variable-name
  "Получение значения для переменной"
  [v]
  (second v))

(defn same-variables?
  "Сравнение переменных"
  [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (=
      (variable-name v1)
      (variable-name v2))))


(defn &&
  "Порожднеие конъюнкции"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::conj (cons expr rest))))

(defn &&?
  "Проверка, является ли выражение конъюнкцией"
  [expr]
  (= ::conj (first expr)))

(defn ||
  "Порожднеие дизъюнкции"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::disj (cons expr rest))))

(defn ||?
  "Проверка, является ли выражение дизъюнкцией"
  [expr]
  (= ::disj (first expr)))

(defn -->
  "Порождение импликации"
  [from to]
  (cons ::impl (list from to)))

(defn ->?
  "Проверка, является ли выражение импликацией"
  [expr]
  (= ::impl (first expr)))

(defn no?
  "Проверка, является ли выражение отрицанием"
  [expr]
  (= ::neg (first expr)))

(defn no
  "Порождение отрицания"
  [expr]
  (if (no? expr)
    (second expr)
    (list ::neg expr)))

(defn args
  "Список аргументов выражения"
  [expr]
  (rest expr))

(defn arg
  "Первый аргумент"
  [expr]
  (second expr))

(defn- get-inside [expr fun]
  (fn [expr-inside] (->> (args expr-inside)
                         (map fun)
                         (cons (first expr)))))

(defn- get-rule [expr rules]
  ((some #(if ((first %) expr)
            (second %)
            false)
         rules) expr))

(defn- not-const-or-var [expr]
  (not (or (constant? expr) (variable? expr))))

(defn print-logic
  "Печать выражения"
  [expr]
  (let [print-rules (list
                      [#(or (constant? %) (variable? %)) #(print (arg %))]
                      [->? #(do (print "(")
                                (print-logic (first (args %)))
                                (print " -> ")
                                (print-logic (second (args %)))
                                (print ")"))]
                      [&&? #(do (print "(")
                                (print-logic (first (args %)))
                                (doall (map (fn [subexpr] (do (print " && ")
                                                              (print-logic subexpr)))
                                            (rest (args %))))
                                (print ")"))]
                      [||? #(do (print "(")
                                (print-logic (first (args %)))
                                (doall (map (fn [subexpr]
                                              (do (print " || ")
                                                  (print-logic subexpr)))
                                            (rest (args %))))
                                (print ")"))]
                      [no? #(do (print "!") (print-logic (first (args %))))])]
    (get-rule expr print-rules)))

(defn println-logic
  "Печать выражения с переводом строки"
  [expr]
  (do (print-logic expr) (println)))

(defn- get-first-disj-from-args [expr]
  (if (empty? expr)
    expr
    (let [first-arg (first expr)]
      (if (||? first-arg)
        first-arg
        (recur (rest expr))))))

(defn- get-args-without-first-disj [expr]
  (lazy-seq (if (empty? expr)
              expr
              (let [first-arg (first expr)
                    rest-args (rest expr)]
                (if (||? first-arg)
                  rest-args
                  (cons first-arg (get-args-without-first-disj rest-args)))))))

(defn simplify-associativity
  "Для аргументов args выражения, удовлетворяющих pred, раскрывает скобки; возвращает примененную операцию oper к модифицированным аргументам"
  [pred oper args]
  (apply oper (reduce (fn [acc arg]
                        (if (pred arg)
                          (concat acc (rest arg))
                          (conj acc arg)))
                      (vec '())
                      args)))

(defn simplify-brackets
  "Раскрыть скобки для одинаковых операторов согласно закону ассоциативности"
  [expr]
  (let [simplify-rules (list
                         ; Для всех аргументов-дизъюнкций раскрыть скобки
                         [||? #(->> (args %)
                                    (map simplify-brackets)
                                    (simplify-associativity ||? ||)
                                    (args)
                                    (apply ||))]
                         ; Для всех аргументов-конъюнкций раскрыть скобки
                         [&&? #(->> (args %)
                                    (map simplify-brackets)
                                    (simplify-associativity &&? &&)
                                    (args)
                                    (apply &&))]
                         ;Заход вглубь
                         [not-const-or-var (get-inside expr simplify-brackets)]
                         [(fn [_] true) (fn [expr] expr)])]
    (get-rule expr simplify-rules)))

(defn distribute
  "Применить закон дистрибутивности"
  [expr]
  (let [simplify-rules (list
                         ; Для первого аргумента-дизъюнкции раскрыть скобки
                         [&&? (fn [inside-expr]
                                (let [args-of-expr (args inside-expr)
                                      first-disj (get-first-disj-from-args args-of-expr)
                                      other-expr (apply && (get-args-without-first-disj args-of-expr))]
                                  (if (empty? first-disj)
                                    (cons (first inside-expr) (map distribute (args inside-expr)))
                                    (->> (args first-disj)
                                         (map #(distribute (&& % other-expr)))
                                         (apply ||)
                                         (distribute)))))]

                         ;Заход вглубь
                         [not-const-or-var (get-inside expr distribute)]
                         [(fn [_] true) (fn [expr] expr)])]
    (get-rule expr simplify-rules)))


(defn simplify-negatives
  "Применить законы де Моргана"
  [expr]
  (let [simplify-rules (list
                         ; && -> ||; || -> &&; выражения заменяются своими отрицаниями
                         [no? (fn [expr] (let [bool-fun (arg expr)]
                                           (if (or (&&? bool-fun) (||? bool-fun))
                                             (let [negative-arguments (map #(simplify-negatives (no %)) (args bool-fun))]
                                               (if (&&? bool-fun)
                                                 (apply || negative-arguments)
                                                 (apply && negative-arguments)))
                                             (no (simplify-negatives bool-fun)))))]

                         ;Заход вглубь
                         [not-const-or-var (get-inside expr simplify-negatives)]
                         [(fn [_] true) (fn [expr] expr)])]
    (get-rule expr simplify-rules)))

(defn simplify-extra-operations
  "Избавиться от всех логических операций, содержащихся в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием"
  [expr]
  (let [simplify-rules (list
                         ; A -> B = !A || B
                         [->? (fn [expr] (let [expr-args (args expr)]
                                           (|| (no (simplify-extra-operations (first expr-args)))
                                               (simplify-extra-operations (second expr-args)))))]
                         ;Заход вглубь
                         [not-const-or-var (get-inside expr simplify-extra-operations)]
                         [(fn [_] true) (fn [expr] expr)])]
    (get-rule expr simplify-rules)))

(defn to-dnf
  "Приведение выражения к ДНФ"
  [expr]
  (->> (simplify-extra-operations expr)
       (simplify-negatives)
       (distribute)
       (simplify-brackets)))

(let [dnf1 (|| (variable :A) (variable :B))
      dnf2 (|| (no (variable :A)) (&& (variable :A) (variable :B)))
      dnf3 (|| (&& (variable :A) (variable :B) (no (variable :C)))
               (&& (no (variable :D)) (variable :E) (variable :F))
               (&& (variable :C) (variable :D))
               (variable :B))
      expr-impl (--> (variable :Z) (--> (variable :X) (variable :Y)))
      expr (no (|| (--> (variable :X) (variable :Y)) (no (--> (variable :Y) (variable :Z)))))]
  (println "DNF:")
  (println-logic dnf1)
  (println "->")
  (println-logic (to-dnf dnf1))
  (println "-------------")
  (println-logic dnf2)
  (println "->")
  (println-logic (to-dnf dnf2))
  (println "-------------")
  (println-logic dnf3)
  (println "->")
  (println-logic (to-dnf dnf3))
  (println "-------------")
  (println)

  (println "Not DNF:")
  (println-logic expr-impl)
  (println "->")
  (println-logic (to-dnf expr-impl))
  (println "-------------")

  (println "Not DNF:")
  (println-logic expr)
  (println "->")
  (println-logic (simplify-extra-operations expr))
  (println "->")
  (println-logic (simplify-negatives (simplify-extra-operations expr)))
  (println "->")
  (println-logic (distribute (simplify-negatives (simplify-extra-operations expr))))
  (println "->")
  (println-logic (to-dnf expr))
  (println "-------------")
  (println))