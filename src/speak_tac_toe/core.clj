(ns speak-tac-toe.core)

(def board-layout [
  [:a1 :b1 :c1]
  [:a2 :b2 :c2]
  [:a3 :b3 :c3]])

(def wins [
  #{:a1 :b1 :c1} #{:a2 :b2 :c2} #{:a3 :b3 :c3} ; rows
  #{:a1 :a2 :a3} #{:b1 :b2 :b3} #{:c1 :c2 :c3} ; columns
  #{:a1 :b2 :c3} #{:a3 :b2 :c1}])              ; diagonal

(defn str->move [s]
  (keyword (clojure.string/lower-case s)))

(defn win? [move-log]
  (let [move-set (into #{} move-log)]
    (some 
      #(clojure.set/subset? % move-set) wins)))

(defn opponent [player]
  (player {:x :o, :o :x}))

(defn move-in-log? [move move-log]
  (some? (some #{move} move-log)))

(defn moves->board [move-log]
  (partition 3 
    (for [row  board-layout
          cell row]
      (move-in-log? cell move-log))))

(defn cell-char [x o]
  (if x "X"
    (if o "O" ".")))

(defn print-row [x-row o-row]
  (apply println (map cell-char x-row o-row)))

(defn print-board [xo-move-log]
  (let [x-board (moves->board (:x xo-move-log))
        o-board (moves->board (:o xo-move-log))]
    (doall (map print-row x-board o-board))))

(defn print-history [xo-move-log]
  (println "X moves:" (:x xo-move-log))
  (println "O moves:" (:o xo-move-log)))

(defn turn [move-log]
  (let [next-move (str->move (read-line))]
    (conj move-log next-move)))

(defn game [player-up xo-move-log]
  ;(print-history xo-move-log)
  (print-board xo-move-log)
  (println "Your move" player-up)
  (let [old-move-log    (player-up xo-move-log)
        new-move-log    (turn old-move-log)
        new-xo-move-log (assoc xo-move-log player-up new-move-log)]
    (if (win? new-move-log)
      player-up
      (recur (opponent player-up) new-xo-move-log))))         

(defn play []
  (let [winner (game :x {:x [] :o []})] 
    (println winner "wins!")))
