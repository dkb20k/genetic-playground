(ns genetic-playground.core
  (:require [clojure.spec :as s]
            [clojure.walk :as walk])
  (:gen-class))

(def predicates ['integer? 'string? 'boolean? '(s/and integer? even?) '(s/and integer? odd?)])
(def seqs ['s/+ 's/*])
(def and-ors ['s/and 's/or])

(def mutate-prob 0.17)
(def crossover-prob 0.4)
(def new-prob 0.34)
(def elite-count 6)

;;

(defn get-score
  [creature test-data]
  (let [problems (::s/problems (s/explain-data (eval (:p creature)) test-data))]
    (if problems
      (assoc creature :fitness (let [p (float (get-in problems [0 :in 0]))
                                   t (count test-data)]
                               (/ p t)))
      (assoc creature :fitness 1.))))

;;

(defn get-random-val
  []
  (rand-nth predicates))

(defn get-random-creature
  [len]
  (let [p (reduce (fn [col i]
                    (conj col (keyword (str i))
                          (get-random-val)))
                  []
                  (range len))]
    `(s/cat ~@p)))

(defn initial-population
  [size max-length]
  (map (fn [_] {:p (get-random-creature (inc (rand-int max-length)))})
       (range size)))

;;

(defn mutable?
  [v]
  (contains? (set predicates) v))

(defn mutate-creature
  [creature]
  (let [p (:p creature)
        p (map (fn [v] (if (and (< (rand) mutate-prob) (mutable? v))
                         (get-random-val)
                         v)) p)]
    (assoc creature :p p)))

;;

(defn crossover
  [creature1 creature2]
  (let [p2 (map #(second %) (partition 2 (next (:p creature2))))
        res (reduce (fn [col x] (conj col
                                      (first x)
                                      (if (< (rand) crossover-prob)
                                        (rand-nth p2))))
                    (partition 2 (next (:p creature1)))
                    [])
        res (apply concat res)
        res `(s/cat ~@res)]
    {:p res}))

;;

(defn select-best [col ts]
  (let [pick (repeatedly ts #(rand-nth col))]
    (last (sort-by :fitness pick))))

(defn perfect-fit
  [col]
  (let [res (filter #(= 1. (:fitness %)) col)
        res (vec res)]
    res))

(defn evolve
  [population-size max-generation ts test-data]
  (loop [gen-number max-generation
         col (initial-population population-size (count test-data))]
    (println "Generation:" gen-number)
    (let [col (map #(get-score % test-data) col)]
      (if (or (zero? gen-number)
              (> (count (perfect-fit col)) 0))
        col
        (let [elite (take elite-count (reverse (sort-by :fitness col)))
              new-col (for [i (range (- (count col) elite-count))]
                        (if (< (rand) new-prob)
                          {:p (get-random-creature (count test-data))}
                          (let [c1 (select-best col ts)
                                c2 (select-best col ts)]
                            (mutate-creature (crossover c1 c2))
                            )))]
          (println "Best fitness" (map :fitness elite))
          (recur (dec gen-number)
                 (into new-col elite)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (evolve 100 100 5 ["hi" true 5 10 "boo" 1 3 6 4 8 5 6 "" "" false]))