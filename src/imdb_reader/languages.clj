(ns imdb-reader.languages
  (:require [imdb-reader.utils :refer :all]))

(defn filter-movies-from-reader
  [language reader]
  (set (for [line (line-seq reader)
             :when (and (re-seq (re-pattern (str "\t" language)) line)
                        (not (or (re-seq #"\(#\d+\.\d+\)" line)
                                 (re-seq #"\s\(TV\)" line)
                                 (re-seq #"\s\(V\)" line)
                                 (re-seq #"\s\{\{SUSPENDED\}\}" line)
                                 (re-seq #"\t\(only a few words\)" line)
                                 (re-seq #"\t\(one line\)" line)
                                 (re-seq #"\t\(dubbed\)" line))))]
         (first (clojure.string/split line #"\t")))))

(defn filter-movies
  [language]
  (with-gzip-file (partial filter-movies-from-reader language) "resources/language.list.gz"))
