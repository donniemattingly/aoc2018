(ns aoc10.core)
(require '[clojure.string :as str])
(require '[clojure.pprint :as pp])

(defn test-input
  []
  (println "Test"))

(defn parse-digits
  [input]
  (apply str 
           (filter #(#{\0,\1,\2,\3,\4,\5,\6,\7,\8,\9,\-,\space,\,} %) input)))

(defn toInt
  [value]
  (Integer/parseInt (str/trim value)))

(defn coordpair->particle
  [coordpair]
  (let [pos (first coordpair)
        dpos (second coordpair)]
    {:x (toInt (first pos)) :y (toInt (second pos)) :dx (toInt (first dpos)) :dy (toInt (second dpos))}))

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


(defn get-bounding-box
  [particles]
  (let [xs (map #(:x %) particles)
        ys (map #(:y %) particles)]
    {:minX (apply min xs) :maxX (apply max xs) :minY (apply min ys) :maxY (apply max ys)}))


(defn get-box-area
  [box]
  (*(- (:maxX box) (:minX box))
    (- (:maxY box) (:minY box))))


(defn advance-particles
  [particles]
  (map (fn [p]
         {:x (+ (:x p) (:dx p)) :y (+ (:y p) (:dy p)) :dx (:dx p) :dy (:dy p)})
       particles))


(defn display-particles
  [particles]
  (let [box (get-bounding-box particles)
        width (+ 1 (- (:maxX box) (:minX box)))
        height (+ 1 (- (:maxY box) (:minY box)))
        particlesvec (vec particles)
        arr  (make-array Long/TYPE width height)]
    (dorun (for [p  particles] 
             (aset-long arr (- (:x p) (:minX box)) (- (:y p) (:minY box)) 1)))
    (let [transposed (apply map list arr)]
      (dorun (for [r transposed] (dorun 
                           (println (for [v r] (if (= v 1) "#" ".")))))))
))

(defn get-particles-with-smallest-area
  [particles oldArea count]
  (let [nextParticles (advance-particles particles)
        box (get-bounding-box nextParticles)
        newArea (get-box-area box)]
    (if 
        (> newArea oldArea) 
      {:p particles :count count} 
      (recur nextParticles newArea (inc count)))))

(defn part-one
  []
  (let [particles (parse-input "input-10.1.txt")]
     (display-particles 
      (:p (get-particles-with-smallest-area particles 
                                            (get-box-area (get-bounding-box particles))
                                            0)))))

(defn part-two
  []
  (let [particles (parse-input "input-10.1.txt")]
     (:count (get-particles-with-smallest-area particles 
                                           (get-box-area (get-bounding-box particles))
                                           0))))

(defn parse-input
  [filename]
  (let [contents (str/split (slurp filename) #"\n")
        coords (map #(parse-line %) contents)]
    coords))

