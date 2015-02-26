(ns imdb-reader.soundtracks
  (:require [imdb-reader.utils :refer :all]
            [imdb-reader.roles :refer :all]))

(def movie-pattern #"^# ")

(def song-pattern #"^- ")

(defn add-movie-key
  [mvs v]
  (let [movie (trim movie-pattern v)]
    (if (mvs movie)
      [movie]
      [])))

(defn add-track-key
  [ks track]
  (if-not (empty? ks)
    [(first ks) (remove-double-quotes (trim song-pattern track))]
    []))

(defn update-keys
  [mvs ks v]
  (cond
    (empty? v) []
    (re-seq movie-pattern v) (add-movie-key mvs v)
    (re-seq song-pattern v) (add-track-key ks v)
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

(defn parse-soundtracks-from-reader
  [movies reader]
  (let [lines (line-seq reader)]
    (loop [mvs movies
           m {}
           ks []
           ls lines]
      (if (empty? ls)
        m
        (let [l (first ls)
              ks (update-keys mvs ks l)]
          (recur mvs (update-map m ks l) ks (rest ls)))))))

(defn parse-soundtracks
  [movies]
  (with-gzip-file (partial parse-soundtracks-from-reader movies) "resources/soundtracks.list.gz"))

