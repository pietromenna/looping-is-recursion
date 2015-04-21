(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [acc bas ex] 
              (if (zero? ex)
                acc
                (recur (* acc bas) bas (dec ex))))]
    (pow 1 base exp)))

(defn last-element [a-seq]
  (let [my-last (fn [a-seq prev] 
                  (if (empty? a-seq)
                    prev
                    (recur (rest a-seq) (first a-seq))))]
    (my-last a-seq nil)))

(defn seq= [seq1 seq2]
  (let [my-comp (fn [seq1 seq2] 
                  (cond 
                    (and (empty? seq1) (empty? seq2)) true
                    (or (empty? seq1) (empty? seq2)) false
                    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                    :else false))]
    (my-comp seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [b-seq a-seq
         idx 0]
    (cond 
      (empty? b-seq) nil
      (pred (first b-seq)) idx
      :else (recur (rest b-seq) (inc idx)))))

(defn avg [a-seq]
  (loop [current a-seq
         sum 0
         entries 0]
    (if (empty? current)
      (/ sum entries)
      (recur (rest current) (+ sum (first current)) (inc entries)))
    ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (loop [result #{}
         b-seq a-seq]
    (if (empty? b-seq)
      result
      (recur (toggle result (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
	(loop [iteration 2
           fib-2 0
           fib-1 1]
   (cond
     (= n 0) 0
     (= n 1) 1
     (= iteration n) (+ fib-2 fib-1)
     :else (recur (inc iteration) fib-1 (+ fib-2 fib-1) ))))

(defn cut-at-repetition [a-seq]
  (loop [result '()
         current a-seq]
    (if (or (empty? current) (some #{(first current)} result))
      (reverse result)
      (recur (cons (first current) result) (rest current)))))

