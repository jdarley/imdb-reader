(ns imdb-reader.utils
  (:require [clojure.java.io :as io]))

(defn trim
  [p s]
  (clojure.string/replace s p ""))

(defn remove-double-quotes
  [s]
  (clojure.string/replace s "\"" ""))

(defn with-gzip-file
  [fn filename]
  (with-open [reader (io/reader
                      (java.util.zip.GZIPInputStream.
                        (io/input-stream filename)))]
    (fn reader)))
