(ns quinto.deck
  (:require [clojure.spec.alpha :as s]
            [quinto.specs :as sp]))

(def MAX-HAND-SIZE 5)

; Going with the tile distribution reported for the marbellized version I played.
; See https://boardgamegeek.com/thread/24859/tile-distribution
(defn make-deck []
  (shuffle
    (apply concat
           (for [[value num-tiles] {0 7
                                    1 6
                                    2 6
                                    3 7
                                    4 10
                                    5 6
                                    ; The poster didn't list any 6s, but I think it may have just
                                    ; been an error on their part?
                                    6 10
                                    7 14
                                    8 12
                                    9 12}]
             (repeat num-tiles value)))))

(s/fdef make-deck
  :args (s/cat)
  :ret ::sp/deck)

(defn draw-tiles [deck hand num-tiles]
  [(drop num-tiles deck)
   (concat hand (take num-tiles deck))])

(s/fdef draw-tiles
  :args (s/cat :deck ::sp/deck :hand ::sp/hand :num-tiles (s/and nat-int? #(<= % MAX-HAND-SIZE)))
  :ret (s/cat :new-deck ::sp/deck :new-hand ::sp/hand))
