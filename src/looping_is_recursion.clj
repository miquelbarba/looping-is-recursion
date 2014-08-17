(ns looping-is-recursion)

(defn power [base exp]
 (let [helper (fn [acc base exp]
                 (if (< exp 1)
                   acc
                   (recur (* acc base) base (dec exp))))]
   (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq current]
                 (if (empty? a-seq)
                   current
                   (recur (rest a-seq) (first a-seq))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper
         (fn [seq1 seq2 is-true]
           (cond
             (not is-true)                           false
             (and (not (empty? seq1)) (empty? seq2)) false
             (and (not (empty? seq2)) (empty? seq1)) false
             (and (empty? seq1) (empty? seq2))       is-true
             :else                                   (recur (rest seq1)
                                                            (rest seq2)
                                                            (= (first seq1) (first seq2)))))]
    (helper seq1 seq2 true)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         coll a-seq]
    (cond
      (empty? coll)       nil
      (pred (first coll)) index
      :else               (recur (inc index) (rest coll)))))

(defn avg [a-seq]
  (loop [num 0
         sum 0
         coll a-seq]
    (if (empty? coll)
      (/ sum num)
      (recur (inc num) (+ sum (first coll)) (rest coll)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [current #{}
         coll a-seq]
    (if (empty? coll)
      current
      (recur (toggle current (first coll)) (rest coll)))))

(defn fast-fibo [n]
  (loop [f-n   1
         f-n-1 0
         index n]
    (cond
      (= 0 index)  0
      (>= 1 index) f-n
      :else        (recur (+ f-n f-n-1) f-n (dec index)))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         current-seq []
         rest-seq a-seq]
    (cond
      (empty? rest-seq)                  current-seq
      (contains? a-set (first rest-seq)) current-seq
      :elae                              (recur (conj a-set (first rest-seq))
                                                (conj current-seq (first rest-seq))
                                                (rest rest-seq)))))

