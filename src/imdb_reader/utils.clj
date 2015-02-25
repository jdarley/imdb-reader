(ns imdb-filter.utils)

(defn trim
  [p s]
  (clojure.string/replace s p ""))

(defn remove-double-quotes
  [s]
  (clojure.string/replace s "\"" ""))
