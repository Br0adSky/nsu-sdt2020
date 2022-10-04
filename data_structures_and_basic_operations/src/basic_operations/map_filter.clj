(ns basic-operations.map_filter)

(defn add-new-word
  "Создает список новых слов из переданного слова"
  [letters word]
  (map #(cons % word) letters))

(defn add-alphabet-to-word
  "Создает список возможных слов"
  [word alphabet]
  (add-new-word (filter #(not (= (first word) %)) alphabet) word))

(defn increment-words
  "Возвращает список слов, увеличенных на алфавит"
  [words alphabet]
  (apply concat (map #(add-alphabet-to-word % alphabet) words)))

(defn iterate-words
  "Увеличивает число слов, пока длина не станет = n"
  [n alphabet] (nth (iterate (fn [word] (increment-words word alphabet))
                             (list (list)))
                    n))