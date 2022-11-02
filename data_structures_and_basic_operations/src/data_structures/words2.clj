(ns data_structures.words2)

(defn add-new-word
  "Добавляет новое слово к списку существующих"
  [letter word words]
  (cons (cons letter word) words))

(defn add-alphabet-to-word
  "Добавляет алфавит к слову"
  [word alphabet]
  (loop [iterated-alphabet alphabet
         result (list)]
   (if (empty? iterated-alphabet)
     result
     (if (= (first word) (first iterated-alphabet))
       (recur (rest iterated-alphabet) result)
       (recur (rest iterated-alphabet) (add-new-word (first iterated-alphabet) word result))))))

(defn increment-words
  "Увелечение всех слов на новую букву из алфавита"
  [words alphabet]
  (loop [iterated-words words
         result (list)]
   (if (empty? iterated-words)
     result
     (recur (rest iterated-words) (concat result (add-alphabet-to-word (first iterated-words) alphabet))))))

(defn iterate-words
  "Проверка соотвествия длины слова"
  [n alphabet]
  (loop [words (add-alphabet-to-word (list) alphabet)
         words-length (dec n)]
   (if (= words-length 0)
     words
     (recur (increment-words words alphabet) (dec words-length)))))