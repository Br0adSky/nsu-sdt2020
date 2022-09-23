(ns data_structures.words2)

(def alphabet '(:a :b :c))

(defn add-new-word
  "Добавляет новое слово к списку существующих"
  [letter word words]
  (cons (cons letter word) words))

(defn add-alphabet-to-word
  "Добавляет алфавит к слову"
  [word]
  (loop [iterated-alphabet alphabet
         result (list)]
   (if (empty? iterated-alphabet)
     result
     (if (= (first word) (first iterated-alphabet))
       (recur (rest iterated-alphabet) result)
       (recur (rest iterated-alphabet) (add-new-word (first iterated-alphabet) word result))))))

(defn increment-words
  "Увелечение всех слов на новую букву из алфавита"
  [words]
  (loop [iterated-words words
         result (list)]
   (if (empty? iterated-words)
     result
     (recur (rest iterated-words) (concat result (add-alphabet-to-word (first iterated-words)))))))

(defn iterate-words
  "Проверка соотвествия длины слова"
  [n]
  (loop [words (add-alphabet-to-word (list))]
   (if (= (count (first words)) n)
     words
     (recur (increment-words words)))))
