(ns aoc10.core)
(require '[clojure.string :as str])

(defn test-input
  []
  (println "Test"))

(defn parse-digits
  [input]
  (apply str 
         (filter #(#{\0,\1,\2,\3,\4,\5,\6,\7,\8,\9,\-,\space,\,} %) input)))

(defn coordpair->particle
  [coordpair]
  (let [pos (first coordpair)
        dpos (second coordpair)]
    {:x (first pos) :y (second pos) :dx (first dpos) :dy (second dpos)}))

(defn parse-line
  [line]
  (let [
        coordRegex #"\<\s*-*\d*,\s*-*\d*\>"
        strCoords (re-find coordRegex line)
        coords  (map 
                 (comp #(str/split % #",") #(parse-digits %)) 
                 (take 2 (re-seq coordRegex line)))
        particles (coordpair->particle coords)
        ]
    particles)
  )

(defn parse-input
  [filename]
  (let [contents (str/split (slurp filename) #"\n")
        coords (map #(parse-line %) contents)]
    (map #(println %) coords)))

