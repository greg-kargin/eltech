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

(def a (atom 0))

(defn bfs-solve [queue target-state visited]
  (swap! a inc)
  (let [current-state (peek queue)
        tail (pop queue)]
    (if (empty? queue)
      :failed-search
      (if (contains? visited (:tiles current-state))
        (recur tail target-state visited)
        (if (= (:tiles current-state) (:tiles target-state))
          (:path current-state)
          (recur (apply conj tail (get-next-states current-state))
                 target-state
                 (conj visited (:tiles current-state))))))))

(defn fixed-depth-dfs [stack target-state visited max-depth]
  (let [[current-state cur-depth] (first stack)
        tail (next stack)]
    (if (empty? stack)
      :failed-search
      (if (or (contains? visited (:tiles current-state))
              (= cur-depth max-depth))
        (recur tail target-state visited max-depth)
        (if (= (:tiles current-state) (:tiles target-state))
          (:path current-state)
          (recur (apply conj tail (map vector (get-next-states current-state) (repeat (inc cur-depth))))
                 target-state
                 (conj visited (:tiles current-state))
                 max-depth))))))

(defn iterative-dfs-solve [stack target-state visited]
  (let [max 1000] ;; max possible
    (loop [depth 0]
      (let [res (fixed-depth-dfs stack target-state visited depth)]
        (cond (< max depth)
              :failed-search

              (keyword? res)
              (recur (inc depth))

              :else
              res)))))

(comment

  (fixed-depth-dfs (list [initial-state 0])
                   target-state
                   #{}
                   2)

  (iterative-dfs-solve (list [initial-state 0])
                       target-state
                       #{})

  (bfs-solve (-> (clojure.lang.PersistentQueue/EMPTY)
                 (conj initial-state))
             target-state
             #{})

  )
