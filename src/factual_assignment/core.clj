(ns factual-assignment.core)

(defn- positions [board]
  (for [r (range (count board))
        c (range (count (get board 0)))]
    [r c]))

(defn- gen-char-map [board]
  (apply (partial merge-with concat)
         (for [pos (positions board)]
           {(get-in board pos) [pos]})))

;; Generall read a file

(def board (atom []))
(def char-map (atom {}))
(def words (atom []))
(def wrap? (atom false)) ;; XXX does this work?

;; Load these in
#_(def board (atom ["FYYHNRD" "RLJCINU" "AAWAAHR" "NTKLPNE" "CILFSAP" "EOGOTPN" "HPOLAND"]))

;; need to update this too
#_(def char-map (atom (gen-char-map @board)))

#_(def words (atom ["ITALY" "HOLLAND" "POLAND" "SPAIN" "FRANCE" "JAPAN" "TOGO" "PERU"]))

(defn pos->char [pos]
  (get-in @board pos))

(defn char->pos [c]
  (@char-map c))

(def route-fn-map
  {:left       (fn [[r c]] [r (dec c)])
   :right      (fn [[r c]] [r (inc c)])
   :up         (fn [[r c]] [(dec r) c])
   :down       (fn [[r c]] [(inc r) c])
   :up-left    (fn [[r c]] [(dec r) (dec c)])
   :up-right   (fn [[r c]] [(dec r) (inc c)])
   :down-left  (fn [[r c]] [(inc r) (dec c)])
   :down-right (fn [[r c]] [(inc r) (inc c)])})

(defn- gen-initial-paths [pos]
  (for [route (set (keys route-fn-map))]
    (let [new-pos ((get route-fn-map route) pos)]
      {:curr (pos->char new-pos)
       :start pos
       :end new-pos
       :route route})))

(defn- initial-candidates [word]
  (let [fc (first word)]
    (apply concat (map #(gen-initial-paths %) (char->pos fc)))))

(defn- step-forward [path]
  (let [route (:route path)
        curr-pos (:end path)
        new-pos ((get route-fn-map route) curr-pos)]
    {:curr (pos->char new-pos)
     :start (:start path)
     :end new-pos
     :route route}))

(defn- step-forward-all [paths]
  (map step-forward paths))

(defn- format-word-result [word paths]
  (when (seq paths)
    (let [path (first paths)]
      {:word word
       :start (:start path)
       :end (:end path)
       :route (:route path)})))

(defn find-word
  ([word] (find-word (rest word) (initial-candidates word) word))
  ([subword candidates word]
   (let [[fc & more] subword
         matching-candidates (filter #(= (:curr %) fc) candidates)]
     ;;(println subword matching-candidates word)
     (if (seq more)
       (find-word more (step-forward-all matching-candidates) word)
       (format-word-result word matching-candidates)))))

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
      (reset! char-map (gen-char-map @board))
      (reset! wrap? wrap-data))
  :ok)

(defn find-words [] (map find-word @words))

;; TODO: Change format before so this isn't necessary
;; Need to fix output print
(defn print-words! [words]
  (doseq [w words]
    (if (seq w)
      (println (format-position (:start w)) (format-position (:end w)))
      (println "NOT FOUND"))))

(defn format-position [[r c]] (str "(" r "," c ")"))


;; It's printing other things but
(print-words! (find-words))

;; To test
(load-data! (parse-data "sample.dat"))


(defn -main [& args]
  (try
    (load-data! (parse-data (first args)))
    (print-words! (find-words))
    (catch Exception e (println "ERROR:" (.getMessage e)))))
