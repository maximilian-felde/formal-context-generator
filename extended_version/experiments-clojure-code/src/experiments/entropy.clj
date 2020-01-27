(ns experiments.entropy
(:require [conexp.fca.contexts :refer :all ]
          [clojure.math.combinatorics :as comb]
          [clojure.math.numeric-tower :as math]
          [conexp.io.contexts :refer :all]
          [clojure.test :refer :all]
          [clojure.set :refer :all]
          [conexp.main :refer :all]
          [clojure.core.reducers :as r]
          [conexp.fca.fast :refer [to-binary-context with-binary-context
                                   bitwise-attribute-derivation
                                   bitwise-object-derivation to-hashset to-bitset]]
          [clojure.math.numeric-tower :refer [expt]])
(:import conexp.fca.implications.Implication
         [java.util ArrayList BitSet LinkedList List ListIterator]
         [clojure.lang IFn PersistentHashSet PersistentVector]))

(with-test
  (defn entropy-m
    "Berechnet die Entropie des Kontexts bezüglich der Merkmale."
    [ctx]
    (let [theattributes (attributes ctx)
          M (count theattributes)
          summands (pmap
                    (fn [x]
                      (- 1
                         (/
                          (count (context-attribute-closure ctx #{x})) M)))
                    theattributes)]
      (/
       (r/fold + summands)
       M)))
    
  (is (= (/ 2 5) (entropy-m (make-context-from-matrix [1 2 3 4 5]
                                                       [1 2 3 4 5]
                                                       [1 1 1 1 1
                                                        0 1 1 1 1
                                                        0 0 1 1 1
                                                        0 0 0 1 1
                                                        0 0 0 0 1]))))
  (is (= (/ 2 3) (entropy-m (make-context-from-matrix [1 2 3 4]
                                                  [1 2 3 4 5 6]
                                                  [1 0 0 0 1 0
                                                   0 1 1 0 1 1
                                                   1 0 0 1 0 1
                                                   0 1 0 1 1 1])))))



(defn entropy-g
  "Berechnet die Entropie des Kontexts bzgl der Objekte."
  [ctx]
  (let [theobjects (objects ctx)
        G (count theobjects)
        summands (pmap
                  (fn [x]
                    (- 1
                       (/
                        (count (context-object-closure ctx #{x})) G)))
                  theobjects)]
    (/
     (r/fold + summands)
     G)))

(defn fast-entropy-g
  "Computes the same entropy as entropy-g does, but through converting the context
  into an binary bitset one."
  [ctx]
  (with-binary-context ctx
    (let [a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count)
          object-derivations    (into-array (pmap (fn [y]
                                                     (let [^BitSet bs (BitSet.)]
                                                       (.set bs y)
                                                       (bitwise-object-derivation
                                                        incidence-matrix
                                                        object-count
                                                        attribute-count
                                                        bs)))
                                                   (range object-count)))
          o-closure-counts (pmap (fn [x]
                                   (.cardinality ^BitSet (a-prime x))) object-derivations)]

      (- 1
         (/
          (r/fold + o-closure-counts)
          (* object-count object-count)))
      )))

(defn log2
  [x]
  (if (= x 0)
    0
    (/ (Math/log x) (Math/log 2))))


(defn fast-shannon-g
  "Computes (fastly) the shannon entropy for formal contexts defined via 1/|G| ∑_g
  -c·log₂(c) where c=|g''|/|G|"
  [ctx]
  (with-binary-context ctx
    (let [a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count)
          object-derivations    (into-array (pmap (fn [y]
                                                     (let [^BitSet bs (BitSet.)]
                                                       (.set bs y)
                                                       (bitwise-object-derivation
                                                        incidence-matrix
                                                        object-count
                                                        attribute-count
                                                        bs)))
                                                   (range object-count)))
          o-closure-counts (pmap (fn [x]
                                   (.cardinality ^BitSet (a-prime x)))
                                 object-derivations)]
      (*
       (/ -1 (* object-count object-count))
       (r/fold + (pmap (fn [x] (*
                               x 
                               (log2 (/ x object-count))))
                       o-closure-counts))))))




(defn entropy
  "Berechnet die Entropie des Kontexts bzgl. der Merkmale und Objekte."
  [kontext]
  (/ (+ (entropy-m kontext) (entropy-g kontext)) 2))

(defn rel-consistency
  "compute relative consistency for a given formal context (G,M,I) and some
  attribute subset N ⊆ M"
  [ctx N]
  (let [obs (objects ctx)
        atts (attributes ctx)
        MN (difference atts N )
        ctx-N (make-context obs N (incidence ctx))
        ctx-MN (make-context obs MN (incidence ctx))]
    (let [derive-N (fn [x] (context-object-closure ctx-N x))
          derive-MN (fn [x] (context-object-closure ctx-MN x))]
      (/
       (count (filter (fn [x] (subset? (derive-N #{x}) (derive-MN #{x}))) obs))
       (count obs)
       ))))


(def mario (make-context-from-matrix ['a 'b 'c 'd]
                                                  ['aa 'bb 'cc 'dd 'ee 'ff]
                                                  [1 0 0 0 1 0
                                                   1 1 1 0 1 1
                                                   1 0 0 1 0 1
                                                   0 1 0 1 1 1]))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))


(defn entropy-log
  [ctx]
  (let [theobjects (objects ctx)
        numberofobj (count theobjects)
        relfrequency (pmap
                      (fn [x] 
                        (/ (count (context-object-closure ctx #{x})) numberofobj)) 
                      theobjects)
        summands (pmap
                  (fn [x]
                    (* x (log2 x)))
                  relfrequency)]
    (/ (- (r/fold + summands)) numberofobj)))

(defn faststarsum
  "Compute the sum |ext(c)| for c in  B*={c ∈ B  | (int(c)∖{m})' = ext(c)}"
  [ctx theconcepts atts]
  (with-binary-context ctx
    (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count)
          a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count)
          theextents (pmap (fn [x] (to-bitset object-vector (first x))) theconcepts)
          theatts (to-bitset attribute-vector atts)]
      (r/fold + (pmap (fn [x] (if
                                  (=
                                   (let [thing (o-prime x)]
                                     (.andNot thing theatts) ;; remove attribute set
                                     (.cardinality (a-prime thing)))
                                   (.cardinality x))
                               (.cardinality x)
                                0))
                        theextents)))))
