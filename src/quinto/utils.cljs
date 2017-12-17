(ns quinto.utils)

(defn remove-item [xs x]
  "Removes the first instance of the value x from the collection xs."
  (cond (empty? xs) xs
        (= x (first xs)) (rest xs)
        :else (cons (first xs) (remove-item (rest xs) x))))

(defn bound-between
  [number lower upper]
  (cond
    (< number lower) lower
    (> number upper) upper
    :else number))
