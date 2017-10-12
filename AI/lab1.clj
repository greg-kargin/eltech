(def initial-state
  {:path []
   :tiles [[:6 :2 :8]
           [:4 :1 :7]
           [:5 :3 :e]]
   :empty-tile [2 2]})

(def target-state
  {:path nil
   :tiles [[:1 :2 :3]
           [:4 :5 :6]
           [:7 :8 :e]]
   :empty-tile [2 2]})

(defn swap-tiles [tiles [x-from y-from] [x-to y-to]]
  (let [from (get-in tiles [x-from y-from])
        to (get-in tiles [x-to y-to])]
    (-> tiles
        (assoc-in [x-from y-from] to)
        (assoc-in [x-to y-to] from))))

(defn neighbours-cords [[x y]]
  (let [possible-states [[:left [x (- y 1)]]
                       [:up [(- x 1) y]]
                       [:down [(+ x 1) y]]
                       [:right [x (+ y 1)]]]]
    (filter (fn [[_ [x y]]] (and (and (<= 0 x)
                                      (<= x 2))
                                 (and (<= 0 y)
                                      (<= y 2))))
            possible-states)))

(defn get-next-states [state]
  (let [n-cords (neighbours-cords (:empty-tile state))]
    (map (fn [[direction to]]
           {:tiles (swap-tiles (:tiles state) (:empty-tile state) to)
            :empty-tile to
            :path (conj (:path state) direction)})
         n-cords)))

(def dfs-heuristic (constantly 0))

(defn bfs-heuristic [state _] (count (:path state)))

(defn misplaced-tiles [state target-state]
  (let [misplaced-in-row (fn [row row'] (reduce + (map (fn [e e'] (if (not= e e') 1 0)) row row')))]
    (+ (count (:path state))
       (reduce + (map misplaced-in-row (:tiles state) (:tiles target-state))))))

(defn new-priority-queue [initial-state target-state picking-function]
  (let [p-q (java.util.PriorityQueue. (comparator (fn [[s _] [s' _]] (< s s'))))]
    (.add p-q [(picking-function initial-state target-state) initial-state])
    p-q))

(defn solve [initial-state target-state picking-function]
  (let [priority-queue (new-priority-queue initial-state target-state picking-function)]
    (loop [visited #{}]
      (if (.isEmpty priority-queue)
        :failed-search
        (let [[score current-state] (.poll priority-queue)]
          (if (contains? visited (:tiles current-state))
            (recur visited)
            (if (= (:tiles current-state) (:tiles target-state))
              (:path current-state)
              (let [next-states (get-next-states current-state)
                    next-states-with-score (map (fn [state]
                                                  [(picking-function state
                                                                     target-state)
                                                   state])
                                                next-states)]
                (doall (map (fn [s] (.add priority-queue s)) next-states-with-score))
                (recur (conj visited (:tiles current-state)))))))))))

(comment

  (solve initial-state target-state misplaced-tiles)

  (solve initial-state target-state bfs-heuristic)

  )
