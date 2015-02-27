(ns imdb-reader.languages
  (:require [imdb-reader.utils :refer :all]))

(defn filter-movies-from-reader
  [language movies reader]
  (zipmap
    (for [line (line-seq reader)
          :when (and (re-seq (re-pattern (str "\t" language)) line)
                     (not (or (re-seq #"\(#\d+\.\d+\)" line)
                              (re-seq #"\s\(TV\)" line)
                              (re-seq #"\s\(V\)" line)
                              (re-seq #"\s\{\{SUSPENDED\}\}" line)
                              (re-seq #"\t\(only a few words\)" line)
                              (re-seq #"\t\(one line\)" line)
                              (re-seq #"\t\(dubbed\)" line))))]
      (first (clojure.string/split line #"\t")))
    (repeat {})))

(def lang-pattern #"\t+([A-Z|a-z|\-| ]+)\t*")

(defn extract-languages-from-reader
  [reader]
  (loop [s #{}
         lines (line-seq reader)]
    (if (empty? lines)
      s
      (let [line (first lines)
            lang (nth (first (re-seq lang-pattern line)) 1)]
        (recur (if (nil? lang) s (conj s lang)) (rest lines))))))

(defn extract-languages []
  (with-gzip-file extract-languages-from-reader "resources/language.list.gz"))

(defn read-languages
  [language movies]
  (with-gzip-file (partial filter-movies-from-reader language movies) "resources/language.list.gz"))
