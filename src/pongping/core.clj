(ns pongping.core
  (:use [relative.rating]
        [relative.trueskill :only [trueskill-engine]]
        [clojure.string :only [split]]))

(def ^:dynamic *match-file* "matches.txt")

(def engine (trueskill-engine))
(defn new-player [name]
  (player engine {:id name}))

(def players (atom #{}))

(defn id->player [players id]
  (some #(when (= id (:id %)) %) players))

(defn maybe-add
  "Add player to players if a player with the same :id does not already exist."
  [players id]
  (if-not (id->player players id)
    (conj players (assoc (new-player id) :games 0 :wins 0))
    players))

(defn update-by-id
  "Updates players set based on :id uniqueness."
  [players player]
  (-> players
      (disj (id->player players (:id player)))
      (conj (update-in player [:games] inc))))

(defn best-matches-for
  "Returns a sequence of [quality player] pairs sorted by match quality."
  [id players]
  (let [p1 (or (id->player players id) (new-player id))]
    (->> (disj players p1)
         (map (fn [p2] [(match-quality engine p1 p2) p2]))
         (sort-by first)
         (reverse))))

(defn id-match
  "Matches two players against each other by id."
  [players id-winner id-loser]
  (let [p1 (update-in (id->player players id-winner) [:wins] inc)
        p2 (id->player players id-loser)]
    (if (and p1 p2)
      (match engine p1 p2)
      (throw (IllegalArgumentException. "Invalid id(s). Does not exist in players.")))))

(defn match-update!
  "Updates a players atom given the result of a match."
  [players result]
  (doseq [p result] (swap! players update-by-id p))
  players)

(defn match! [players-atom id-winner id-loser]
  (match-update! players-atom (id-match @players-atom id-winner id-loser)))

(defn do-matches!
  [players matches]
  (doseq [[p1 p2] matches]
    (swap! players maybe-add p1)
    (swap! players maybe-add p2)
    (match! players p1 p2)))

(defn matches-from-file! [players filename]
  (do-matches! players (partition 2 (split (slurp filename) #"\s"))))

(defn print-stats []
  (reset! players #{})
  (matches-from-file! players *match-file*)
  (let [stats [["Name"    #(str (:id %))]
               [" Rating" #(format " %.2f" (rating %))]
               ["Mean"    #(format "%.2f" (:mean %))]
               ["Std-Dev" #(format "%.2f" (:std-dev %))]
               ["#Games"  #(str (:games %))]
               ["Win%" #(format "%.2f"
                                (float (/ (:wins %)
                                          (:games %))))]]
        players (reverse (sort-by #(rating %) @players))
        header (apply str (interpose "\t" (map first stats)))
        divider (apply str (repeat (+ 7 (count header)) "-"))]
    (println header)
    (println divider)
    (doseq [player players]
      (->> (interpose "\t" (map (fn [[_ func]]
                                  (func player))
                               stats))
           (apply str)
           (println)))))

(defn find-matches
  ([id] (find-matches id 5))
  ([id n]
     (reset! players #{})
     (matches-from-file! players *match-file*)
     (let [matches (take n (best-matches-for id @players))
           formatted (map (fn [[quality player]]
                            (str (:id player)
                                 "\t"
                                 (format " %.4f" quality)))
                          matches)]
       (println "Name\t Quality")
       (println (apply str (repeat 16 "-")))
       (doseq [match formatted]
         (println match)))))
