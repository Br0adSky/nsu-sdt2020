(ns basic-operations.map_filter)
(def alphabet '(:a :b :c))

(defn add-new-word
  "Создает список новых слов из переданного слова"
  [letters word]
  (map #(cons % word) letters))

(defn add-alphabet-to-word
  "Создает список возможных слов"
  [word]
  (add-new-word (filter #(not (= (first word) %)) alphabet) word))

(defn increment-words
  "Возвращает список слов, увеличенных на алфавит"
  [words]
  (apply concat (map #(add-alphabet-to-word %) words)))

(defn iterate-words
  "Увеличивает число слов, пока длина не станет = n"
  [n] (nth (iterate increment-words (list (list))) n))