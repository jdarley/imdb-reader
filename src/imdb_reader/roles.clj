(ns imdb-reader.roles)

(def artist-join-pattern #"(,\s|\sand\s|\s&\s|\sFeat\.\s)")

(def known-artist-pattern #"\'*(.+?)[\s\(I+|V+|X+\)]*\'")

(def string-to-role
  {"additional lyrics" [:lyricist]
   "additional vocals" [:performer]
   "arrange and composed" [:composer]
   "arranged and produced" [:composer :producer]
   "arranged" [:composer]
   "by" [:performer]
   "compose" [:composer]
   "composed and arranged" [:composer]
   "composed and directed" [:composer]
   "composed and lyrics" [:composer :lyricist]
   "composed and performed by" [:performer :composer]
   "composed and performed" [:performer :composer]
   "composed and sang" [:performer :composer]
   "composed and sung" [:performer :composer :lyricist]
   "composed and written" [:composer :lyricist]
   "composed by" [:composer]
   "composed" [:composer]
   "composed & performed" [:performer :composer]
   "composer" [:composer]
   "composition" [:composer]
   "co-written" [:composer :lyricist]
   "director" [:director]
   "english lyrics" [:lyricist]
   "featuring" [:performer]
   "(female reprise version) sung" [:performer]
   "female vocals" [:performer]
   "hindi lyrics" [:lyricist]
   "hindi/urdu lyrics" [:lyricist]
   "instrumental" [:performer]
   "lyric" [:lyricist]
   "lyrics and composed" [:composer :lyricist]
   "lyrics and music" [:composer :lyricist]
   "lyrics by" [:lyricist]
   "lyrics & composed" [:composer :lyricist]
   "lyrics" [:lyricist]
   "lyrics & music" [:lyricist :composer]
   "lyrics written" [:lyricist]
   "main vocals" [:performer]
   "(male reprise version) sung" [:performer]
   "muisc" [:composer]
   "music and lyrics" [:composer :lyricist]
   "music composed" [:composer]
   "music" [:composer]
   "music composer" [:composer]
   "music directed" [:director]
   "music director" [:director]
   "music produced" [:producer]
   "music written" [:composer]
   "new lyrics" [:lyricist]
   "original lyrics" [:lyricist]
   "original music" [:composer]
   "original music written" [:composer]
   "peformed" [:performer]
   "penned" [:composer :lyricist]
   "perfoemd" [:performer]
   "performed and arranged" [:performer :composer]
   "performed and composed" [:performer :composer]
   "performed and lyrics" [:performer :lyricist]
   "performed and music" [:performer :composer]
   "performed by" [:performer]
   "performed & composed" [:performer :composer]
   "performed, lyrics and composed" [:performer :composer :lyricist]
   "performed, lyrics & composed" [:performer :composer :lyricist]
   "performed, lyrics, composed" [:performer :composer :lyricist]
   "performed & lyrics" [:performer :lyricist]
   "performed & music" [:performer :composer]
   "performed" [:performer]
   "performed & produced" [:performer :producer]
   "performef" [:performer]
   "playback" [:performer]
   "playback singer" [:performer]
   "produced and distributed" [:producer]
   "produced and performed" [:performer :producer]
   "produced and published" [:producer]
   "produced and written" [:composer :lyricist :producer]
   "produced by" [:producer]
   "produced" [:producer]
   "rap" [:performer]
   "recited" [:performer]
   "remixed" [:producer]
   "remix" [:producer]
   "shabad written by" [:lyricist]
   "singer" [:performer]
   "singers" [:performer]
   "spoken word performance" [:performer]
   "sung and composed" [:performer :composer :lyricist]
   "sung and written" [:performer :composer :lyricist]
   "sung by" [:performer]
   "sung, composed and lyrics" [:performer :composer :lyricist]
   "sung" [:performer]
   "tamil lyrics" [:lyricist]
   "tra. and arr." [:composer]
   "trad. arranged and play" [:performer :composer]
   "(unplugged version) sung" [:performer]
   "urdy lyrics" [:lyricist]
   "vocal" [:performer]
   "vocals and music performed" [:performer]
   "vocals" [:performer]
   "whistled" [:performer]
   "words" [:lyricist]
   "words & music" [:composer :lyricist]
   "wriiten" [:lyricist]
   "written and performed and produced" [:performer :composer :lyricist :producer]
   "written and performed by" [:performer :composer :lyricist]
   "written and performed" [:performer :composer :lyricist]
   "written and produced" [:composer :lyricist :producer]
   "written and sang" [:performer :composer :lyricist]
   "written and sung" [:performer :composer :lyricist]
   "written by" [:lyricist]
   "written, composed and performed" [:performer :composer :lyricist]
   "written, composed and sang" [:performer :composer :lyricist]
   "written" [:lyricist]
   "written & performed" [:performer :composer :lyricist]
   "written, produced and performed" [:performer :composer :lyricist :producer]
   "writter" [:lyricist]})

(def role-pattern #"^\s+(.*?)(\s[Bb]y|:)\s(.*)")

(defn extract-roles
  [s]
  (let [role (nth (first (re-seq role-pattern s)) 1)]
    (when-not (nil? role)
      (if (empty? role)
        [:performer]
        (string-to-role (.toLowerCase role))))))

(defn remove-clutter
  [name]
  (if (re-seq #"^\'" name)
    (nth (first (re-seq known-artist-pattern name)) 1)
    name))

(defn extract-names
  [s]
  (let [names (nth (first (re-seq role-pattern s)) 3)]
    (vec (map remove-clutter (clojure.string/split names artist-join-pattern)))))

(defn add-roles
  [m ks vs]
  (loop [m m
         vs (set vs)
         ks (map #(conj (vec (take 2 ks)) %) (last ks))]
    (if (empty? ks)
      m
      (let [path (first ks)]
        (recur
          (if (get-in m path)
            (update-in m path clojure.set/union vs)
            (assoc-in m path vs))
          vs (rest ks))))))
