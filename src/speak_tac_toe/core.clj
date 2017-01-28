(ns speak-tac-toe.core)

(def wins [
  #{:a1 :b1 :c1} #{:a2 :b2 :c2} #{:a3 :b3 :c3} ; rows
  #{:a1 :a2 :a3} #{:b1 :b2 :b3} #{:c1 :c2 :c3} ; columns
  #{:a1 :b2 :c3} #{:a3 :b2 :c1}])              ; diagonal


(defn str->move [s]
  (keyword (clojure.string/lower-case s)))

(defn win? [move-log]
  (let [moves (into #{} move-log)]
    (some 
      #(clojure.set/subset? % moves) wins)))

(defn turn [move-log]
  (let [next-move (str->move (read-line))]
    (conj move-log next-move)))

(defn opponent [player]
  (player {:x :o 
           :o :x}))

(defn game [player-up xo-move-log]
  (println "Your move" player-up)
  (let [old-move-log (player-up xo-move-log)
        new-move-log (turn old-move-log)]
    (if (win? new-move-log)
      player-up
      (recur 
        (opponent player-up)
        (assoc xo-move-log player-up new-move-log)))))
         

(defn play []
  (let [winner (game :x {:x [] :o []})] 
    (println winner "wins!")))
