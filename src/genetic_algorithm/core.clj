(ns genetic-algorithm.core)

;
; Infinite lazy sequence of bits
;
(defn random-bits []
  (lazy-seq (cons (rand-int 2) (random-bits))))

(defn new-bit-string-chromosome [size] 
  (take size (random-bits)))

;
; Infinite lazy seq of chromosomes
;
(defn new-population [chromosome-cons score-fitness]
  (lazy-seq
    (let [newc (chromosome-cons)]
      (cons {:chr newc :fitness (score-fitness newc)} (new-population chromosome-cons score-fitness)))))

(defn new-bitstring-population [chromosome-len pop-size score-fitness]
  (take pop-size (new-population #(new-bit-string-chromosome chromosome-len) score-fitness)))

(defn greater-fitness? [x y]
  (> (:fitness x) (:fitness y)))

(defn sort-pop [population]
  (sort greater-fitness? population))

(defn add-fitness [accum chromosome]
  (+ accum (:fitness chromosome)))

(defn sum-fitnesses [pop]
  (reduce add-fitness 0 pop))


(defn spin-roulette-wheel
  "The population is sorted by fitness so walk through the
  population accumulating the fitnsses as we go. Once they
  exceed the random number, we use the head of the population
  as the chosen chromosome."
  [total-fitness pop]
  (let [roll (rand total-fitness) accumulator 0]
      (loop [_acc accumulator, _pop pop]
        (cond (empty? _pop) (throw (Exception. "Unexpected end of population"))
              (< roll (+ _acc (:fitness (first _pop))))     (first _pop)
              :else (recur (+ _acc (:fitness (first _pop))) (rest _pop))))))

(defn flip-bit [bit] (if (= bit 0) 1 0))

(defn mutate-bit [bit bit-mutation?]
  (if (bit-mutation?) (flip-bit bit) bit))

(defn mutate-chromosome [chromosome bit-mutation?]
  (map #(mutate-bit % bit-mutation?) chromosome))

; clen=6
; 012345
; 1 to 5
; 1 + (rand-int (clen - 2))

(defn swap-loc [chromosome-len]
  "Create a random swap location in the chromosome. Exclude swapping
  at the very beginning or the very end which is a wasted operation
  resulting in unchanged location."
  (if (< chromosome-len 2) (throw (Exception. "Single bit chromosomes are meaningless")))
  (+ 1 (rand-int (- chromosome-len 1))))

(defn crossover [c1 c2 crossover-point]
  (loop [index 0 offspring1 () offspring2 () pairs (map vector c1 c2)]
    (if (empty? pairs) [ (reverse offspring1) (reverse offspring2) ]
        (recur (inc index)
               (conj offspring1 (get (first pairs) (if (< index crossover-point) 0 1)))
               (conj offspring2 (get (first pairs) (if (< index crossover-point) 1 0)))
               (rest pairs)))))

(defn crossover-chromosomes-randomly [c1 c2]
  "Return a vector pair of chromosomes such that 70% of the time
  they have their genetic material swapped at a random point.
  30% of the time, they are returned unmodified."
  (if (> (rand) 0.7)
    [c1 c2]
    (crossover c1 c2 (swap-loc (count c1)))))

(defn make-offspring
  "Spin the roulette wheel and choose two chromosomes to
  combine. Apply the mutation and crossover functions
  and return the result."
  [chromosome-source mutation-func crossover-func]
  (let [pair (crossover-chromosomes-randomly (chromosome-source) (chromosome-source))]
    (mutation-func (nth pair 0) (nth pair 1))))

;
; All-ones GA
;
(defn count-occurrences [coll x]
  "Count the occurrences of x in coll"
  (count (filter #(= % x) coll)))

(defn more-ones? [a b]
  (> (count-occurrences a 1) (count-occurrences b 1)))

(defn new-ten-bit-chr []
  (new-bit-string-chromosome 10))

(defn count-ones-score-fitness [coll]
  (count-occurrences coll 1))

(defn bit-mutation? [rate] (< (rand) rate))

;
; Build a new population of the same size with combined mutants
; chosen (favoring the most fit) from the given population.
; It is assumed that the population is pre-sorted by fitness.
;
(defn next-generation [population]
  (let [pop-size (count population) tot-fitness (sum-fitnesses population)]
    (println "pop-size " pop-size " tot-fitness " tot-fitness)
    (defn chromosome-source [] (:chr (spin-roulette-wheel tot-fitness population)))
    (loop [i 0 coll ()] 
      (if (>= i pop-size)
        coll
        (recur (inc i) (conj coll (make-offspring chromosome-source mutate-chromosome crossover-chromosomes-randomly)))))))

;;
;; UTILITIES
;;
(defn print-pop [population]
  (println "POPULATION:")
  (loop [p population]
    (when (not-empty p)
      (println "Chromosome: " (:chr (first p)) " Fitness: " (:fitness (first p)))
      (recur (rest p)))))

;
; Run the algorithm
;
(defn ga [num-generations population]
  (sort-pop
    (loop [gen 0 p population]
      (if (>= gen num-generations) p (recur (inc gen) (next-generation (sort-pop p)))))))

(def orig-pop (sort-pop (new-bitstring-population 10 5 count-ones-score-fitness)))
(print-pop orig-pop)
(print-pop (next-generation orig-pop))


