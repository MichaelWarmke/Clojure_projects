(ns pw-gen.core
    (:gen-class)
    (:require [clojure.string :as str]))

(defn isSuitableWord [word]
      (let [wordString (str word)
            wordCount (count wordString)
            distinctCount (count (distinct (str/split wordString #"")))]
           (and
             (> (count wordString) 4)
             (< (count wordString) 6)
             (< (- wordCount distinctCount) 2))))


(defn injectRandomSpecial [specialChars pw]
      (let [pwString (str pw)
            [begin end] (split-at
                          (rand-int (count pwString))
                          (str/split pwString #""))
            rndChar (rand-nth (str/split specialChars #""))]
           (concat begin rndChar end)))

(defn getSuitableWords [path]
      (filter #(isSuitableWord %)
              (map #(str/trim %)
                (str/split (slurp path) #"\n"))))

(defn -main [& args]

      (let [wordsFile "./resources/words_alpha.txt"
            specialChars "!@#$%^&*()_?><{}[]:"
            wordSeparator "-"
            suitableWords (getSuitableWords wordsFile)]

           (def passWithoutChar
              (str/join
               wordSeparator
               (take 3 (shuffle suitableWords))))

           (println (reduce #(str %1 %2) (injectRandomSpecial specialChars passWithoutChar)))))

