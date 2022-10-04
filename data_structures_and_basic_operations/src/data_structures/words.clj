(ns data_structures.words
  (:gen-class))


(defn add-new-word
  "Добавляет новое слово к списку существующих"
  [letter word words]
  (cons (cons letter word) words))

(defn add-alphabet-to-word
  "Добавляет алфавит к слову"
  ([word alphabet] (add-alphabet-to-word word alphabet (list)))
  ([word alphabet1 result]
   (if (empty? alphabet1)
     result
     (if (= (first word) (first alphabet1))
       (add-alphabet-to-word word (rest alphabet1) result)
       (add-alphabet-to-word word (rest alphabet1) (add-new-word (first alphabet1) word result))))))

(defn increment-words
  "Увелечение всех слов на новую букву из алфавита"
  ([words alphabet] (increment-words words (list) alphabet))
  ([words result alphabet]
   (if (empty? words)
     result
     (increment-words (rest words) (concat result (add-alphabet-to-word (first words) alphabet)) alphabet))))

(defn iterate-words
  "Проверка соотвествия длины слова"
  ([n alphabet] (iterate-words (dec n) (add-alphabet-to-word (list) alphabet) alphabet))
  ([n words alphabet]
   (if (= 0 n)
     words
     (iterate-words (dec n) (increment-words words alphabet) alphabet))))