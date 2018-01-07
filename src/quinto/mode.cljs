(ns quinto.mode
  "Functions for transitioning the app's state in response to user input.

  At any given time, the app will be in one of these three modes:
    * :default - the default mode; displays the current state of the board.
    * :assembling-move - can be entered from :default mode. Allows the player
      to build up a move, tile by tile.
    * :viewing-historical-move - can be entered from either mode. Allows the player
      to view past moves; if the player made a move that was non-optimal, allows
      the player to see what their optimal move would have been. "
  (:require [com.rpl.specter :refer [select ALL LAST]]
            [quinto.ai :as ai]
            [quinto.deck :as deck]
            [quinto.grid :as g]
            [quinto.utils :refer [remove-item]]))

(declare make-ai-move)

(defn fresh-game-state []
  (let [initial-state {:grid                      g/empty-grid
                       :deck                      (deck/make-deck)
                       :player-scores             []
                       :player-hand               []
                       :ai-scores                 []
                       :ai-hand                   []
                       :most-recent-computer-move []
                       :mode                      {:mode/type :default}
                       :game-over                 false}

        [new-deck player-hand] (deck/draw-tiles (initial-state :deck) [] deck/MAX-HAND-SIZE)
        [new-deck ai-hand] (deck/draw-tiles new-deck [] deck/MAX-HAND-SIZE)

        state (-> initial-state
                  (assoc :deck new-deck)
                  (assoc :player-hand player-hand)
                  (assoc :ai-hand ai-hand))]

    (if (rand-nth [true false])
      (make-ai-move state)
      state)))

;;; Helper functions

(defn can-go-back? [state]
  (and (not (state :game-over))
       (= (get-in state [:mode :mode/type]) :assembling-move)))

(defn can-confirm-move? [state]
  (and (not (state :game-over))
       (= (get-in state [:mode :mode/type]) :assembling-move)
       (g/is-grid-valid? (state :grid))
       (> (count (get-in state [:mode :move-so-far]))
          0)))

(defn can-select-a-tile? [state]
  (and (not (state :game-over))
       (some? (get-in state [:mode :selected-cell]))))

;;; State transition functions

(defn enter-assembling-move-mode
  "Used when the user selects a green (playable) cell in order to begin
  assembling a move. Creates an :assembling-move mode dict which records
  the selected cell and is ready for further user input."
  [state selected-cell]
  (assert (contains? (set (g/find-playable-cells (state :grid)))
                     selected-cell))
  (assoc state
         :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far     []
          :tentative-score nil
          :original-hand   (state :player-hand)
          :original-grid   (state :grid)}))

(defn select-cell
  "Used when the board is already in assembling-move mode and the user
  has selected another cell they'd like to include in their move."
  [state cell]
  (assoc-in state [:mode :selected-cell] cell))

(defn select-tile
  "Used when the board is in assembling-mode, the user has previously selected
  a tile to make a move on, and is now selecting a value to place on that tile."
  [state value]
  (assert (some? (get-in state [:mode :selected-cell])))

  (let [[x y] (get-in state [:mode :selected-cell])]
    (as-> state $
      (assoc-in $ [:grid x y] value)
      (assoc-in $ [:mode :selected-cell] nil)
      (update-in $ [:player-hand] remove-item value)
      (update-in $ [:mode :move-so-far] conj [[x y] value])
      (assoc-in $ [:mode :tentative-score]
                (g/score-move (get-in $ [:mode :original-grid])
                              (get-in $ [:mode :move-so-far])))
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))))

(defn cancel-mode
  "Cancels out of assembling-mode and puts the board back into default mode."
  [state]
  (cond-> state
    (contains? (state :mode) :original-grid) (assoc :grid (get-in state [:mode :original-grid]))
    (contains? (state :mode) :original-hand) (assoc :player-hand (get-in state [:mode :original-hand]))
    true (assoc :mode {:mode/type :default})))

(defn go-back
  "Basically an undo button for assembling-move mode - backs out the most-recently-added
  part of the under-assembly move."
  [state]
  (assert (not= (get-in state [:mode :mode/type])
                :default))

  (cond
    ; If the player has only selected a cell and hasn't actually
    ; placed any tiles, then "backing out" means: "show's over, go back to :default mode."
    (and (some? (get-in state [:mode :selected-cell]))
         (= (count (get-in state [:mode :move-so-far])) 0))
    (cancel-mode state)

    ; If the player has already placed some tiles and currently has a cell selected,
    ; un-select that cell and we're done.
    (some? (get-in state [:mode :selected-cell]))
    (as-> state $
      (assoc-in $ [:mode :selected-cell] nil)
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))

    ; If there's no selected cell, then we need to remove the most-recently-placed
    ; tile and put it back in the player's hand.
    (seq (get-in state [:mode :move-so-far]))
    (let [[[x y] value] (last (get-in state [:mode :move-so-far]))]
      (as-> state $
        (assoc-in $ [:grid x y] nil)
        (update-in $ [:mode :move-so-far] pop)
        (assoc-in $ [:mode :tentative-score]
                  (g/score-move (get-in $ [:mode :original-grid])
                                (get-in $ [:mode :move-so-far])))
        (update-in $ [:player-hand] conj value)
        (assoc-in $ [:mode :available-cells]
                  (g/find-next-open-cells-for-move
                    ($ :grid)
                    (get-in $ [:mode :move-so-far])))
        (assoc-in $ [:mode :selected-cell] [x y])))))

(defn make-ai-move
  "Causes the computer opponent to make a move."
  [state]
  (let [move (ai/pick-move (state :grid) (state :ai-hand))
        move-tiles (select [ALL LAST] move)
        spent-hand (reduce remove-item (state :ai-hand) move-tiles)
        [new-deck new-hand] (deck/draw-tiles (state :deck)
                                             spent-hand
                                             (count move-tiles))]
    (-> state
        (update-in [:ai-scores] conj {:value (g/score-move (state :grid) move)
                                      :move  move
                                      :grid  (state :grid)})
        (assoc :grid (g/make-move (state :grid) move))
        (assoc :most-recent-computer-move move)
        (assoc :ai-hand new-hand)
        (assoc :deck new-deck))))

(defn maybe-end-game
  "Ends the game if the player is unable to make a move."
  [state]
  (if (and (seq (state :player-hand))
           (ai/pick-move (state :grid) (state :player-hand)))
    state
    (assoc state :game-over true)))

(defn confirm-move
  "Used when the board is in assembling-move mode and a valid move has been assembled.
  Applies the move to the board, then has the AI make a move. Updates both players' scores."
  [state]
  (let [move (get-in state [:mode :move-so-far])
        move-tiles (select [ALL LAST] move)
        [new-deck new-hand] (deck/draw-tiles (state :deck)
                                             (state :player-hand)
                                             (count move-tiles))

        grid (get-in state [:mode :original-grid])
        optimal-move (ai/pick-move grid (get-in state [:mode :original-hand]))
        optimal-score (g/score-move grid optimal-move)
        move-score (g/score-move grid move)

        new-state (-> state
                      (assoc :grid (g/make-move grid move))
                      (assoc :mode {:mode/type :default})
                      (update-in [:player-scores]
                                 conj
                                 {:value        move-score
                                  :grid         grid
                                  :move         move
                                  :optimal-move (when (not= move-score optimal-score)
                                                  optimal-move)})
                      (assoc :deck new-deck)
                      (assoc :player-hand new-hand)
                      make-ai-move
                      maybe-end-game)]

    (assert (g/is-grid-valid? (new-state :grid)))
    new-state))

(defn view-historical-move
  "Displays a previous move on the board."
  [state grid move optimal-move]
  (let [original-state (if (= (get-in state [:mode :mode/type]) :viewing-historical-move)
                         (get-in state [:mode :original-state])
                         state)]

    (-> state
        (assoc :most-recent-computer-move [])
        (assoc :grid (g/make-move grid move))
        (assoc :mode {:mode/type      :viewing-historical-move
                      :move           move
                      :optimal-move   optimal-move
                      :original-state original-state}))))

(defn stop-viewing-historical-move
  "Backs out of :viewing-historical-move mode."
  [state]
  (if (= (get-in state [:mode :mode/type]) :viewing-historical-move)
    (get-in state [:mode :original-state])
    state))
