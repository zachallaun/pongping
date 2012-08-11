(ns pongping.core
  (:use [relative.rating]
        [relative.trueskill :only [trueskill-engine]]))

(def engine (trueskill-engine))
(defn new-player [name]
  (player engine {:id name}))

(def players (atom #{(new-player "Zach")
                     (new-player "James")
                     (new-player "Nick")
                     (new-player "Nathan")
                     (new-player "Loftie")
                     (new-player "Derrick")
                     (new-player "Mary")}))

(defn id->player [players id]
  (some #(when (= id (:id %)) %) players))

(defn update-by-id
  "Updates players set based on :id uniqueness."
  [players player]
  (-> players
      (disj (id->player players (:id player)))
      (conj player)))

(defn best-matches-for
  "Returns a sequence of [quality player] pairs sorted by match quality."
  [id players]
  (let [p1 (id->player players id)]
    (->> (disj players p1)
         (map (fn [p2] [(match-quality engine p1 p2) p2]))
         (sort-by first)
         (reverse))))

(defn id-match
  "Matches two players against each other by id."
  [players id-winner id-loser]
  (let [p1 (id->player players id-winner)
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
