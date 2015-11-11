(ns factual-assignment.core)

;; Game state
;; XXX: could put this in one game atom / opts and also make more functions pure

(def board (atom []))
(def char-map (atom {}))
(def dims (atom {}))
(def words (atom []))
(def wrap? (atom false))

;; Main API to game state

(defn pos->char [pos] (get-in @board pos))
(defn char->pos [c] (@char-map c))
(defn size [dim] (get @dims dim))

;; Util functions

(defn- positions [board]
  (for [r (range (count board))
        c (range (count (get board 0)))]
    [r c]))

(defn- gen-char-map
  "Character map for quick lookup."
  [board]
  (apply (partial merge-with concat)
         (for [pos (positions board)]
           {(get-in board pos) [pos]})))

(defn- maybe-wrap
  "If wrapping is set, wrap n to the other side of a row or col."
  [step-fn n dim]
  (let [x (step-fn n)]
    (if @wrap?
      (mod x (size dim))
      x)))

;; Loading and parsing

(defn parse-data [file]
  (let [lines (clojure.string/split (slurp file) #"\n")
        [m n] (map #(Integer. %) (clojure.string/split (first lines) #" "))
        board (vec (take m (rest lines)))
        wrap? (= "WRAP" (first (drop (+ m 1) lines)))
        nwords (Integer. (first (drop (+ m 2) lines)))
        words (vec (drop (+ m 3) lines))]
    [board words wrap?]))

(defn load-data! [[board-data words-data wrap-data]]
  (do (reset! board board-data)
      (reset! words words-data)
      (reset! wrap? wrap-data))
  (reset! dims {:row (count @board) :col (count (get @board 0))})
  (reset! char-map (gen-char-map @board)))

;; Main logic

(def route-fn-map
  "Map from routes to functions updating positions."
  {:left       (fn [[r c]] [r (maybe-wrap dec c :col)])
   :right      (fn [[r c]] [r (maybe-wrap inc c :col)])
   :up         (fn [[r c]] [(maybe-wrap dec r :row) c])
   :down       (fn [[r c]] [(maybe-wrap inc r :row) c])
   :up-left    (fn [[r c]] [(maybe-wrap dec r :row) (maybe-wrap dec c :col)])
   :up-right   (fn [[r c]] [(maybe-wrap dec r :row) (maybe-wrap inc c :col)])
   :down-left  (fn [[r c]] [(maybe-wrap inc r :row) (maybe-wrap dec c :col)])
   :down-right (fn [[r c]] [(maybe-wrap inc r :row) (maybe-wrap inc c :col)])})

(defn- gen-initial-paths [pos]
  (for [route (set (keys route-fn-map))]
    (let [new-pos ((get route-fn-map route) pos)]
      {:curr (pos->char new-pos)
       :start pos
       :end new-pos
       :route route})))

(defn initial-candidates [word]
  (apply concat (map #(gen-initial-paths %) (char->pos (first word)))))

(defn step-forward [path]
  (let [new-pos ((get route-fn-map (:route path)) (:end path))]
    {:curr (pos->char new-pos)
     :start (:start path)
     :end new-pos
     :route (:route path)}))

(defn find-word
  "Find a word on a board."
  ([word] (find-word (rest word) (initial-candidates word) word))
  ([subword candidates word]
   (let [[fc & more] subword
         matching-candidates (remove #(= (:end %) (:start %))
                                     (filter #(= (:curr %) fc) candidates))]
     (if (seq more)
       (find-word more (map step-forward matching-candidates) word)
       (first matching-candidates)))))

;; Format, printing and main

(defn format-position [[r c]] (str "(" r "," c ")"))

(defn print-words! [words]
  (doseq [w words]
    (if (seq w)
      (println (format-position (:start w)) (format-position (:end w)))
      (println "NOT FOUND"))))

(defn -main [& args]
  (try (load-data! (parse-data (first args)))
       (print-words! (map find-word @words))
       (catch Exception e (println "ERROR:" (.getMessage e)))))
