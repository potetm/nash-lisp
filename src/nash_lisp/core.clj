(ns nash-lisp.core)

(def ability-modifier
  {1 -5, 2 -4, 3 -4, 4 -3, 5 -3, 6 -2, 7 -2, 8 -1, 9 -1, 10 0,
   11 0, 12 1, 13 1, 14 2, 15 2, 16 3, 17 3, 18 4, 19 4, 20 5})

(defrecord Abilities [strength
                      dexterity
                      constitution
                      wisdom
                      intelligence
                      charisma])

(defn abilities [m]
  {:pre [(every? #(<= 1 %1 20) (vals m))]}
  (let [abilities (reduce
                    (fn [m k]
                      (assoc m
                        k (get m k 10)))
                    m
                    [:strength
                     :dexterity
                     :constitution
                     :wisdom
                     :intelligence
                     :charisma])]
    (map->Abilities abilities)))

(defrecord EvercraftCharacter [name
                               state
                               alignment
                               armor-class
                               hit-points
                               abilities
                               experience-points])

(defn character [{:keys [name
                         state
                         alignment
                         armor-class
                         hit-points
                         abilities
                         experience-points]}]
  {:pre [(some? name)
         (#{:good :evil :neutral} alignment)
         (<= armor-class 20)]}
  (let [armor-class (or armor-class 10)
        hit-points (or hit-points 5)
        state (or state :alive)
        experience-points (or experience-points 1)]
    (assert (#{:dead :alive} state))
    (->EvercraftCharacter name
                          state
                          alignment
                          armor-class
                          hit-points
                          abilities
                          experience-points)))

(defn level [{:keys [experience-points] :as _character}]
  (inc (Math/floorDiv experience-points 1000)))

(comment (character {:name "a"
                     :alignment :good
                     :armor-class 12
                     :hit-points 11}))

(defn roll []
  (inc (rand-int 19)))

(defn attack [{{:keys [strength]} :abilities :as attacker}
              {{:keys [dexterity constitution]} :abilities :as attacked}
              roll-val]
  (let [roll-val (+ roll-val (Math/floorDiv (level attacker) 2))
        armor (+ (:armor-class attacked) (ability-modifier dexterity))
        attack-strength (+ roll-val (ability-modifier strength))
        success? (< armor attack-strength)
        critical? (= roll-val 20)
        {:keys [hit-points] :as new-attacked}
        (update-in attacked [:hit-points]
                   (fn [v]
                     (let [hit-points (+ (if critical?
                                           (- v (+ 2 (* 2 strength)))
                                           (- v (+ 1 strength)))
                                         constitution
                                         (* 5 (level attacked)))]
                       (if (pos? hit-points)
                         hit-points
                         1))))
        new-attacker (update-in attacker [:experience-points] (partial + 10))]
    (if success?
      [new-attacker
       (assoc new-attacked :state (if (<= hit-points 0)
                                    :dead
                                    :alive))]
      [attacker attacked])))
