(ns pw-gen.core
    (:gen-class)
    (:require [clojure.string :as str]))

(defn isSuitableWord [num word]
  (let [wordString    (str word)
        wordCount     (count wordString)
        distinctCount (count (distinct (str/split wordString #"")))]
    (and
     (> (count wordString) (- num 1))
     (< (count wordString) (+ num 1))
     (< (- wordCount distinctCount) 2))))


(defn injectRandomSpecial [specialChars pw]
  (let [pwString    (str pw)
        [begin end] (split-at
                     (rand-int (count pwString))
                     (str/split pwString #""))
        rndChar     (rand-nth (str/split specialChars #""))]
    (concat begin rndChar end)))

(defn getSuitableWords [num path]
  (filter #(isSuitableWord num %)
          (map #(str/trim %)
               (str/split (slurp path) #"\n"))))

(defn -main [& args]

  (let [wordsFile     "./resources/words_alpha.txt"
        commonWordsFile "./resources/3k_common.txt"
        specialChars  "!@#$%^&*()_?><{}[]:"
        wordSeparator "-"
        fiveLetterWords (getSuitableWords 5 wordsFile)]

    (def passWithoutChar
      (str/join
       wordSeparator
       (take 3 (shuffle fiveLetterWords))))

    (println
      (str/join
       ""
       (take 4 (shuffle (getSuitableWords 4 commonWordsFile)))))


    (println
      (reduce #(str %1 %2) (injectRandomSpecial specialChars passWithoutChar)))))

