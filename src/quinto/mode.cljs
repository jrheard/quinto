(ns quinto.mode
  (:require [com.rpl.specter :refer [select ALL LAST]]
            [quinto.ai :as ai]
            [quinto.deck :as deck]
            [quinto.grid :as g]
            [quinto.utils :refer [remove-item]]))

; TODO DOCUMENT THIS MODULE AND PROBABLY SOME OR MOST OF THE FUNCTIONS
(defn enter-assembling-move-mode [state selected-cell]
  (assert (contains? (set (g/find-playable-cells (state :grid)))
                     selected-cell))

  (assoc state
         :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far     []
          :original-hand   (state :player-hand)
          :original-grid   (state :grid)}))

(defn select-cell [state cell]
  (assert (nil? (get-in state [:mode :selected-cell])))

  (-> state
      (assoc-in [:mode :selected-cell] cell)
      (assoc-in [:mode :available-cells] [])))

(defn select-tile [state value]
  (assert (some? (get-in state [:mode :selected-cell])))

  (let [[x y] (get-in state [:mode :selected-cell])]
    (as-> state $
      (assoc-in $ [:grid x y] value)
      (assoc-in $ [:mode :selected-cell] nil)
      (update-in $ [:player-hand] remove-item value)
      (update-in $ [:mode :move-so-far] conj [[x y] value])
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))))

(defn cancel-mode [state]
  (cond-> state
    (contains? (state :mode) :original-grid) (assoc :grid (get-in state [:mode :original-grid]))
    (contains? (state :mode) :original-hand) (assoc :player-hand (get-in state [:mode :original-hand]))
    true (assoc :mode {:mode/type :default})))

(defn go-back [state]
  (assert (not= (get-in state [:mode :mode/type])
                :default))

  (cond
    (and
      (some? (get-in state [:mode :selected-cell]))
      (= (count (get-in state [:mode :move-so-far]))
         0))
    (-> state
        (assoc :grid (get-in state [:mode :original-grid]))
        (assoc :mode {:mode/type :default}))

    (some? (get-in state [:mode :selected-cell]))
    (as-> state $
      (assoc-in $ [:mode :selected-cell] nil)
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))

    (seq (get-in state [:mode :move-so-far]))
    (let [[[x y] value] (last (get-in state [:mode :move-so-far]))]

      (-> state
          (assoc-in [:grid x y] nil)
          (update-in [:mode :move-so-far] pop)
          (update-in [:player-hand] conj value)
          (assoc-in [:mode :available-cells] [])
          (assoc-in [:mode :selected-cell] [x y])))))

(defn -make-ai-move [state]
  (let [move (ai/pick-move (state :grid) (state :ai-hand))
        move-tiles (select [ALL LAST] move)
        spent-hand (reduce remove-item (state :ai-hand) move-tiles)
        ; xxxx deck function
        [new-deck new-hand] (deck/draw-tiles (state :deck)
                                             spent-hand
                                             (count move-tiles))]
    (-> state
        (update-in [:ai-scores] conj (g/score-move (state :grid) move))
        (assoc :grid (g/make-move (state :grid) move))
        (assoc :ai-hand new-hand)
        (assoc :deck new-deck))))

(defn confirm-move [state]
  (let [move (get-in state [:mode :move-so-far])
        move-tiles (select [ALL LAST] move)
        ; xxxx deck function here too
        [new-deck new-hand] (deck/draw-tiles (state :deck)
                                             (state :player-hand)
                                             (count move-tiles))]
    (-> state
        (assoc :grid (g/make-move (get-in state [:mode :original-grid])
                                  move))
        (assoc :mode {:mode/type :default})
        (update-in [:player-scores]
                   conj
                   (g/score-move (get-in state [:mode :original-grid])
                                 move))
        (assoc :deck new-deck)
        (assoc :player-hand new-hand)
        -make-ai-move)))
