(ns data_structures.words
  (:gen-class))

(def alphabet '(:a :b :c))

(defn add-new-word
  "Добавляет новое слово к списку существующих"
  [letter word words]
  (cons (cons letter word) words))

(defn add-alphabet-to-word
  "Добавляет алфавит к слову"
  ([word] (add-alphabet-to-word word alphabet (list)))
  ([word alphabet1 result]
   (if (empty? alphabet1)
     result
     (if (= (first word) (first alphabet1))
       (add-alphabet-to-word word (rest alphabet1) result)
       (add-alphabet-to-word word (rest alphabet1) (add-new-word (first alphabet1) word result))))))

(defn increment-words
  "Увелечение всех слов на новую букву из алфавита"
  ([words] (increment-words words (list)))
  ([words result]
   (if (empty? words)
     result
     (increment-words (rest words) (concat result (add-alphabet-to-word (first words)))))))

;n тоже можно через def вынести
(defn iterate-words
  "Проверка соотвествия длины слова"
  ([n] (iterate-words n (add-alphabet-to-word (list))))
  ([n words]
   (if (= (count (first words)) n)
     words
     (iterate-words n (increment-words words)))))
