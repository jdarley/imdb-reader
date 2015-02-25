(ns imdb-filter.core
  (:require [imdb-filter.roles :refer :all]
            [imdb-filter.utils :refer :all]
            [cheshire.core :refer :all]))

(def movie-pattern #"^# ")

(def song-pattern #"^- ")

(defn filter-movies-by-language
  [language]
  (set (for [line (line-seq (clojure.java.io/reader "resources/language.list"))
             :when (and (re-seq (re-pattern (str "\t" language)) line)
                        (not (or (re-seq #"\(#\d+\.\d+\)" line)
                                 (re-seq #"\s\(TV\)" line)
                                 (re-seq #"\s\(V\)" line)
                                 (re-seq #"\s\{\{SUSPENDED\}\}" line)
                                 (re-seq #"\t\(only a few words\)" line)
                                 (re-seq #"\t\(one line\)" line)
                                 (re-seq #"\t\(dubbed\)" line))))]
         (first (clojure.string/split line #"\t")))))

(defn add-track
  [ks track]
  (if-not (empty? ks)
    [(first ks) (remove-double-quotes (trim song-pattern track))]
    []))

(defn add-movie
  [mvs v]
  (let [movie (trim movie-pattern v)]
    (if (mvs movie)
      [movie]
      [])))

(defn update-keys
  [mvs ks v]
  (cond
    (empty? v) []
    (re-seq movie-pattern v) (add-movie mvs v)
    (re-seq song-pattern v) (add-track ks v)
    :else (if (>= (count ks) 2)
            (let [ks (vec (take 2 ks))]
              (if-let [roles (extract-roles v)]
                (conj ks roles)
                ks))
            [])))

(defn update-map
  [m ks v]
  (if (= (count ks) 3)
    (let [names (extract-names v)]
      (add-roles m ks names))
    m))

(defn parse-soundtracks
  [language lines]
  (loop [mvs (filter-movies-by-language language)
         m {}
         ks []
         ls lines]
    (if (empty? ls)
      m
      (let [l (first ls)
            ks (update-keys mvs ks l)]
        (recur mvs (update-map m ks l) ks (rest ls))))))

(defn create-tracks
  [m]
  (vec (map #(merge {:track (first %)} (second %)) m)))

(defn create-representation
  [m]
  (map #(let [movie (first %)
              tracks (second %)]
         {:movie  movie
          :tracks (create-tracks tracks)}) m))

(defn read-soundtracks
  []
  (let [lines (line-seq (clojure.java.io/reader "resources/soundtracks.list"))]
    (parse-soundtracks "Hindi" lines)))

(defn go []
  (let [soundtracks (read-soundtracks)]
    (spit "/home/jdarley/Desktop/bollywood-tracks.json" (generate-string (vec (create-representation soundtracks))))))
