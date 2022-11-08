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

(defn print-logic
  "Печать выражения"
  [expr]
  (let [print-rules (list
                      [(fn [expr] (or (constant? expr) (variable? expr))) (fn [expr] (print (arg expr)))]
                      [->? (fn [expr] (do (print "(")
                                          (print-logic (first (args expr)))
                                          (print " -> ")
                                          (print-logic (second (args expr)))
                                          (print ")")))]
                      [&&? (fn [expr] (do (print "(")
                                          (print-logic (first (args expr)))
                                          (doall (map (fn [subexpr] (do (print " && ")
                                                                        (print-logic subexpr)))
                                                      (rest (args expr))))
                                          (print ")")))]
                      [||? (fn [expr] (do (print "(")
                                          (print-logic (first (args expr)))
                                          (doall (map (fn [subexpr]
                                                        (do (print " || ")
                                                            (print-logic subexpr)))
                                                      (rest (args expr))))
                                          (print ")")))]
                      [no? (fn [expr] (do (print "!") (print-logic (first (args expr)))))])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           print-rules)
     expr)))

(defn println-logic
  "Печать выражения с переводом строки"
  [expr]
  (do (print-logic expr) (println)))

; Служебное
(defn- get-first-disj-from-args [expr]
  (if (empty? expr)
    expr
    (let [first-arg (first expr)]
      (if (||? first-arg)
        first-arg
        (recur (rest expr))))))

; Служебное
(defn- get-args-without-first-disj [expr]
  (if (empty? expr)
    expr
    (let [first-arg (first expr)]
      (if (||? first-arg)
        (rest expr)
        (cons first-arg (get-args-without-first-disj (rest expr)))))))

(defn simplify-associativity
  "Для аргументов args выражения, удовлетворяющих pred, раскрывает скобки; возвращает примененную операцию oper к модифицированным аргументам"
  [args pred oper]
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
                         [||? (fn [expr] (apply || (args (simplify-associativity (map simplify-brackets (args expr)) ||? ||))))]
                         ; Для всех аргументов-конъюнкций раскрыть скобки
                         [&&? (fn [expr] (apply && (args (simplify-associativity (map simplify-brackets (args expr)) &&? &&))))]

                         ; Заход вглубь
                         [(fn [expr] (not (or (constant? expr) (variable? expr)))) #(cons (first expr) (map simplify-brackets (args %)))]
                         [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           simplify-rules)
     expr)))

(defn distribute
  "Применить закон дистрибутивности"
  [expr]
  (let [simplify-rules (list
                         ; Для первого аргумента-дизъюнкции раскрыть скобки
                         [&&? (fn [expr] (let [first-disj (get-first-disj-from-args (args expr))
                                               other-expr (apply && (get-args-without-first-disj (args expr)))]
                                           (if (empty? first-disj)
                                             (cons (first expr) (map distribute (args expr)))
                                             (distribute (apply || (map (fn [expr] (distribute (&& expr other-expr))) (args first-disj)))))))]

                         ; Заход вглубь
                         [(fn [expr] (not (or (constant? expr) (variable? expr)))) #(cons (first expr) (map distribute (args %)))]
                         [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           simplify-rules)
     expr)))


(defn simplify-negatives
  "Применить законы де Моргана"
  [expr]
  (let [simplify-rules (list
                         ; && -> ||; || -> &&; выражения заменяются своими отрицаниями
                         [no? (fn [expr] (if (or (&&? (arg expr)) (||? (arg expr)))
                                           (let [negative-arguments (map (fn [expr] (simplify-negatives (no expr))) (args (arg expr)))]
                                             (if (&&? (arg expr))
                                               (apply || negative-arguments)
                                               (apply && negative-arguments)))
                                           (no (simplify-negatives (arg expr)))))]

                         ; Заход вглубь
                         [(fn [expr] (not (or (constant? expr) (variable? expr)))) #(cons (first expr) (map simplify-negatives (args %)))]
                         [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           simplify-rules)
     expr)))

(defn simplify-extra-operations
  "Избавиться от всех логических операций, содержащихся в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием"
  [expr]
  (let [simplify-rules (list
                         ; A -> B = !A || B
                         [->? (fn [expr] (|| (no (simplify-extra-operations (first (args expr))))
                                             (simplify-extra-operations (second (args expr)))))]

                         ; Заход вглубь
                         [(fn [expr] (not (or (constant? expr) (variable? expr))))
                          #(cons (first expr) (map simplify-extra-operations (args %)))]
                         [(fn [expr] true) (fn [expr] expr)])]
    ((some (fn [rule]
             (if ((first rule) expr)
               (second rule)
               false))
           simplify-rules)
     expr)))

(defn to-dnf
  "Приведение выражения к ДНФ"
  [expr]
  (simplify-brackets (distribute (simplify-negatives (simplify-extra-operations expr)))))


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