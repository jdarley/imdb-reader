(ns imdb-reader.core
  (:require [imdb-reader.languages :refer :all]
            [imdb-reader.soundtracks :refer :all]
            [cheshire.core :refer :all]))

(defn create-tracks
  [m]
  (vec (map #(merge {:track (first %)} (second %)) m)))

(defn create-representation
  [m]
  (map #(let [movie (first %)
              tracks (second %)]
         {:movie  movie
          :tracks (create-tracks tracks)}) m))

(defn go []
  (let [movies (filter-movies "Bengali")
        soundtracks (parse-soundtracks movies)]
    (spit "/home/jdarley/Desktop/bengali-tracks.json" (generate-string (vec (create-representation soundtracks)) {:pretty true}))))
