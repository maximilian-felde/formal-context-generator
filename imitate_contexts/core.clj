(ns clojure-random-contexts.core
  (:gen-class)
  (:require [conexp.fca.contexts :refer :all ]
            [conexp.fca.implications :refer :all]
            [conexp.io.contexts :refer :all]
            [conexp.base :refer [set-of exists forall =>]]
            [clojure.java.io           :as io]
            [clojure.set :refer [subset?]]
            [clojure.math.numeric-tower :refer :all]
            
      )
  (:import [java.io File]
           [org.apache.commons.math3.distribution GammaDistribution EnumeratedIntegerDistribution UniformIntegerDistribution EnumeratedDistribution]
           [org.apache.commons.math3.util Pair]
           )
)




(defn normalizeVector
  "given positive(!) vector v return v/||v||_1"
  [v]
  {:pre [(every? #(>= % 0) v)
         (some #(> % 0 ) v)]}
  (let [ sum (reduce + v)] (map #(/ % sum) v))
)


(defn makeContext
  "generate random context for given number of attributes, objects and 
  a vector containing the numbers of attributes for each object"
  [num_attr num_objects obj_attr_numbers]
  (make-context-from-matrix
   (vec (range num_objects))
   (vec (range num_attr))
   (flatten (map #(shuffle (into (vec (take %1 (repeat 1))) (vec (take (- num_attr %1) (repeat 0))))) 
                 obj_attr_numbers)))
)


(defn createGammaDistr
   "returns a sample of a gamma distribution with shape alpha and scale 1"
   ([] (.sample (GammaDistribution. 1 1)))
   ([alpha] (if (> alpha 0) (.sample (GammaDistribution. alpha 1)) 0))
)

(defn createDirichletDistr
  "
  create a dirichlet distribution for a given list of shape-values
  shape is expected to be a non-empty vector with some values > 0
  scale is expected to be a scalar > 0
  "
  [& {shape :shape 
    scale :scale 
    :or {shape [1] 
         scale 1}}]
  {:pre [(some #(> % 0) shape)
         (> scale 0)]}
  (let [alpha (map #(* scale %) shape)
        gamma_rvs (map createGammaDistr alpha) 
        gamma_rvs_sum (reduce + gamma_rvs) 
        dirichlet_rvs (map #(/ % gamma_rvs_sum) gamma_rvs)]
    dirichlet_rvs))


(defn createCategoricalDistribution
  "given a vector of categories and a probabilities vector returns a categorical distribution"
  ([categories-probabilities-vector]
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) categories-probabilities-vector))
   )

  ([categories probabilities]
   {:pre [(= (count categories) (count probabilities))
          (vector? categories)
          (vector? probabilities)]}
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) (map vector categories probabilities)))))


(defn createCategoricalDistributionFromDirichlet
  "
  create categorical distribution based on a dirichlet distribution with :shape @shape and :scale @scale
  shape is expected to be a non-empty vector with values > 0
  scale is expected to be a scalar > 0  
  "
  [& {shape :shape 
      scale :scale 
      :or {shape [1] 
           scale 1}}] 
  (let [dirichlet_rvs (double-array (createDirichletDistr :shape shape :scale scale))
        values (int-array (range (count dirichlet_rvs)))]
    (EnumeratedIntegerDistribution. values dirichlet_rvs)
    ))

;; todo add shape_scale parameter for unaltered dirichlet distr.
(defn randomDirichletContext
  "
  generate a formal context with @num_attributes attributes
  and a random number of objects 
  based on a dirichlet distribution with shape (1 ... 1) and scale 0.1
  optional parameters: 
  :num_objects - number of objects
  :shape - the shape parameter vector (will be normalized to ||.||_1 = 1)
  :scale - the scale parameter
  "
  [num_attributes & {num_objects :num_objects shape :shape scale :scale scale-shape :scale-shape}]
  {:pre [(or (vector? shape) (nil? shape))
         (or (number? num_attributes))
         (or (number? num_objects) (nil? num_objects))
         (or (number? scale) (nil? scale))
         (if (vector? shape) (= (count shape) (+ 1 num_attributes)) (nil? shape))
         (or (vector? scale-shape) (nil? scale-shape))
         ]}
  (let [num_objects (if num_objects num_objects (.sample (UniformIntegerDistribution. num_attributes (expt 2 num_attributes))))
        shape (if shape shape  (vec (take (+ 1 num_attributes) (repeat 1))))
        shape (normalizeVector shape)
        scale (if scale scale (* 0.1 (count shape)))
        cat_dist (createCategoricalDistributionFromDirichlet :shape shape :scale scale)
        obj_attr_numbers (for [i (range num_objects)] (.sample cat_dist))] 
    (makeContext num_attributes num_objects obj_attr_numbers)
    ))


(defn testRandomDirichletContext 
  []
  (let [x (clarify-context (randomDirichletContext 7  :scale 1000))
        ix (intents x)
        pix (canonical-base x)
        ]
    (do
      (println x)
      (println "#intents" (count ix))
      (println "#pseudo-intents" (count pix))
      (println ix)
      (println pix)
      )))


(defn imitateContext-with-dirichlet
      "imitate using dirichlet"
      [n ctx]
      (let [num_attr (count (attributes ctx)) 
            num_objects (count (objects ctx))
            freq (frequencies (map #(count (object-derivation ctx [%])) (objects ctx))) 
            shape (->> (range (+ 1 num_attr)) (map #(freq %)) (map #(if % % 0)) (vec))
            scale (*  1000 num_attr)]
        (for [i (range n)] (randomDirichletContext num_attr :num_objects num_objects :shape shape :scale scale) 
             )))
    
    
(defn imitateContext-with-categorical
      "imitate using relative frequencies as probabilities of categorical distr"
      [n ctx]
      (let [num_attr (count (attributes ctx)) 
               num_objects (count (objects ctx))
               freq (frequencies (map #(count (object-derivation ctx [%])) (objects ctx))) 
                                        ; freqs are being normalized internally ... (should be done before)
               cat_distr (createCategoricalDistribution freq)
               ]
        (for [i (range n)] (makeContext num_attr num_objects (for [i (range num_objects)] (.sample cat_distr))))
           ))


(defn imitateContext-with-shuffle
      "imitate using shuffle"
      [n ctx]
      (let [num_attr (count (attributes ctx))
            num_objects (count (objects ctx))
            obj_attr_numbers  (map (fn [obj] (-> obj (#(object-derivation ctx [%])) (count))) (objects ctx))]
        (for [i (range n)]  (makeContext num_attr num_objects obj_attr_numbers))
        ))


(defn randomContext
      ""
      [n num_attributes]
      (map (fn [in] 
             (let [ctx (randomDirichletContext num_attributes)
                       I (count (intents ctx))
                       PI (count (canonical-base ctx))]
                   [I PI]
                     )
             ) (range n)))


(defn ctx->I-PI
  "computes the number of intents and pseudo-intents (I-PI-coordinate) for a context"
  [ctx]
  (let [I (count (intents ctx))
        PI (count (canonical-base ctx))]
  [I PI]  
  ))


(defn min-max-I-PI
  ""
  [I-PI-list]
  (reduce (fn [result value] 
            (let [minI (min (:minI result) (first value))
                  maxI (max (:maxI result) (first value))
                  minPI (min (:minPI result) (second value))
                  maxPI (max (:maxPI result) (second value))]
              {:minI minI :maxI maxI :minPI minPI :maxPI maxPI}))
          {:minI (first (first I-PI-list))
           :maxI (first (first I-PI-list))
           :minPI (second (first I-PI-list))
           :maxPI (second (first I-PI-list))} 
          I-PI-list))


(defn results-to-file
  [results directory]
  (with-open [wrtr (clojure.java.io/writer directory)]
    (doseq [result results]
      (.write wrtr (str (result 0) ";" (result 1) ";" (result 2) "\n")))))


(defn obj-attr-node-degrees
  ""
  [ctx]
  (let [num_attr (count (attributes ctx))
        freq (frequencies (map #(count (object-derivation ctx [%])) (objects ctx)))
        shape (->> (range (+ 1 num_attr)) (map #(freq %)) (map #(if % % 0)) (vec))]
    shape))

(defn node-degree-distribution
  ""
  [ctx]
  (let [degrees (obj-attr-node-degrees ctx)
        sum (reduce + degrees)]
    (map #(/ % sum) degrees)))



(defn node-degrees-to-file
  ""
  [list-of-degree-dists directory]
  (with-open [wrtr (clojure.java.io/writer directory)]
    (binding [*print-length* false
              *out* wrtr]
      (pr list-of-degree-dists
        ))))


(def ctx-tealady (read-context "./resources/tealady.cxt"))

(do
  (println ctx-tealady)
  (println (map #(format "%.3f" %) (map double (node-degree-distribution ctx-tealady))))
  (def sim-n 1000)
  (println (map #(format "%.3f" %) (map double (map #(/ % sim-n) (apply map + (map node-degree-distribution (imitateContext-with-dirichlet sim-n ctx-tealady))))))))

