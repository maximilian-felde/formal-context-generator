(ns experiments.read-headerless-binary-csv
  "Implements IO for binary csv files (headless)"
  (:require 
   [conexp.base :refer [defalias illegal-argument unsupported-operation set-of]]
   [conexp.fca.contexts :refer [make-context]]
   [conexp.io.contexts :refer [define-context-input-format read-context]]
   [conexp.io.util :refer [define-format-dispatch with-out-writer with-in-reader get-line get-lines]]
   [clojure.string      :refer (split)])
  (:import [java.io PushbackReader]))


(define-context-input-format :headerless-binary-csv
  [file]
  (with-in-reader file
     (let [first-line (split (read-line) #",")
          atts       (range (count first-line))]
      (loop [objs      #{0},
             incidence (set-of [0 n] | n atts, :when (= (nth first-line n) "1"))]
        (if-let [line (read-line)]
          (let [line (split line #","),
                i    (count objs)]
            (recur (conj objs i)
                   (into incidence
                         (for [n atts :when (= (nth line n) "1")]
                           [i n]))))
          (make-context objs atts incidence))))))
