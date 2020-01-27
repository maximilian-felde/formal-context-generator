(ns experiments.context-statistics
  (:require [conexp.io.contexts :as cxtio]
            [conexp.fca.contexts :as cxt]
            [conexp.fca.implications :as impl]
            [conexp.fca.fast :as cxtfast]
            [clojure.set :refer :all]
            [clojure.pprint :refer [pprint]]
            [experiments.read-headerless-binary-csv]
            [experiments.entropy :as entropy]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.core.async :as async]
            ))

(defn contains-maximal-contranominal? [cxt]
  "checks if a context contains the biggest contranominal scale"
  (let [attr (cxt/attributes cxt)
        contranominal (into #{} (for [x attr] (difference attr #{x})))
        attr-sets-cxt (into #{} (for [x (cxt/objects cxt)] (cxt/object-derivation cxt #{x})))]
    (empty? (difference contranominal attr-sets-cxt))))

(defn density [cxt]
  (let [num_inc (count (cxt/incidence cxt))
        num_all (* (count (cxt/attributes cxt))
                   (count (cxt/objects cxt)))]
    (/ num_inc num_all)))

(defn concepts-fast [cxt]
  (cxtfast/concepts cxt))

(defn values->Distribution
  [coll]
  (let
      [freq (frequencies coll)
       probs (into {} (for [[k v] freq] [k (/ v (reduce + (vals freq)))]))]
    probs))


(defn freqs->Distribution
  [freqs]
  (let
      [probs (into {} (for [[k v] freqs] [k (/ v (reduce + (vals freqs)))]))]
    probs))

(defn row-sum-distribution
  [cxt]
  (let  [values  (for [g (cxt/objects cxt)] (count (cxt/object-derivation cxt #{g})))
         zero-freqs (into {} (for [i (range (inc (count (cxt/attributes cxt))))] [i 0]))
         freqs (into zero-freqs (frequencies values))]
    (freqs->Distribution freqs)))   

(defn column-sum-distribution
  [cxt]
  (let  [values  (for [m (cxt/attributes cxt)] (count (cxt/attribute-derivation cxt #{m})))
         zero-freqs (into {} (for [i (range (inc (count (cxt/objects cxt))))] [i 0]))
         freqs (into zero-freqs (frequencies values))]
    (freqs->Distribution freqs)))   

#_(defn column-sum-distribution
  [cxt]
  (values->Distribution (for [m (cxt/attributes cxt)] (count (cxt/attribute-derivation cxt #{m})))))

(defn object-hull-size-distribution
  "computes distribution of the sizes of object concepts, i.e., g''"
  [cxt]
  (let  [values  (for [g (cxt/objects cxt)] (count (cxt/context-object-closure cxt #{g})))
         zero-freqs (into {} (for [i (range (inc (count (cxt/attributes cxt))))] [i 0]))
         freqs (into zero-freqs (frequencies values))]
    (freqs->Distribution freqs)))


(defn num-concepts [cxt]
  (if (contains-maximal-contranominal? cxt)
    (expt 2 (count (cxt/attributes cxt)))
    (count (concepts-fast cxt))))

(defn implications-fast [cxt]
  (if (contains-maximal-contranominal? cxt)
    #{}
    (cxtfast/canonical-base cxt)))

(defn average
  [numbers]
    (if (empty? numbers)
      0
      (/ (reduce + numbers) (count numbers))))

(defn num-distinct-objects [cxt]
  (count (cxt/objects (cxt/clarify-objects cxt))))

(defn ratio-distinct-objects [cxt]
  (/ (count (cxt/objects (cxt/clarify-objects cxt))) (count (cxt/objects cxt))))

(defn distribution-of-objects-in-concepts [objects concepts]
  (let [extents (map first concepts)]
    (for [o objects] [o (count (filter true? (for [e extents] (subset? #{o} e))))])))

(defn sorted-sequence-objects-in-concepts-count [objects concepts]
  (into [] (sort > (map second (distribution-of-objects-in-concepts objects concepts)))))

(defn get-filename-and-directory [file]
  (#(str (.getName (.getParentFile %)) "/" (.getName %)) file))

(defn get-filename [file]
  (#(str (.getName %)) file))

(defn get-generatorname [file]
  (#(str (.getName (.getParentFile %))) file))

(defn file->cxt [file & {:keys [format] :or {format :burmeister}} ]
  (->> file
       (str ,,,)
       (#(cxtio/read-context % format) ,,,)))

(defn cxt-from-file-to-hashmap [file & {:keys [format] :or {format :burmeister}}]
  (->> file
       (#(hash-map :file %
                   :name (get-filename %)
                   :generator (get-generatorname %)
                   :cxt (file->cxt % :format format)),,,)))

(def concepts-and-implications 
  {:concepts #(concepts-fast (:cxt %))
   :canonical-base #(implications-fast (:cxt %))})

(def stats-based-on-concepts-and-implications 
  {:num-concepts #(count (:concepts %))
   :objects-in-concepts-count #(sorted-sequence-objects-in-concepts-count (cxt/objects (:cxt %)) (:concepts %))
   :num-pseudo-intents #(count (:canonical-base %))})

(def functions 
  {:num-attributes #(count (cxt/attributes (:cxt %)))
   :num-objects #(count (cxt/objects (:cxt %)))
   :row-sum-distribution #(row-sum-distribution (:cxt %))
   :column-sum-distribution #(column-sum-distribution (:cxt %))
   :object-hull-size-distribution #(object-hull-size-distribution (:cxt %))
   :density #(format "%.5f" (double (density (:cxt %))))
   :num-distinct-objects #(num-distinct-objects (:cxt %))
   :ratio-distinct-objects #(format "%.5f" (double (ratio-distinct-objects (:cxt %))))
   :shannon-entropy #(entropy/fast-shannon-g (:cxt %))
   :entropy #(entropy/fast-entropy-g (:cxt %))}) 

(defn function-results-to-hashmap [hashmap map-of-functions]
  (into {} (map (fn [[k v]] [k (v hashmap)]) map-of-functions)))


(defn add-concepts-and-implications-to-hashmap
  [hashmap]
  (into hashmap (function-results-to-hashmap hashmap concepts-and-implications)))

(defn add-stats-based-on-concepts-and-implications-to-hashmap
  [hashmap]
  (into hashmap (function-results-to-hashmap hashmap stats-based-on-concepts-and-implications)))

(defn add-function-results-to-hashmap
  [hashmap]
  (into hashmap (function-results-to-hashmap hashmap functions)))

(def compute-statistics (comp add-function-results-to-hashmap 
                              add-stats-based-on-concepts-and-implications-to-hashmap  
                              add-concepts-and-implications-to-hashmap))

(def compute-cheap-statistics add-function-results-to-hashmap)

;;; ouput to csv

;; (def living-beings (cxtio/read-context "./res/living-beings-and-water.ctx"))

;; (defn write-csv [path row-data & {:keys [columns]}]
;;   (let [headers (map name columns)
;;         rows (map #(map % columns) row-data)]
;;     (with-open [file (io/writer path)]
;;       (csv/write-csv file (cons headers rows)))))

;; (def data
;;   [{:ax "Completed"   :b 1 :c "Friday"  :d 4}
;;    {:ax "Started"     :b 1 :c "Monday"  :d 4}
;;    {:ax "In Progress" :b 1 :c "Sunday"  :d 1}
;;    {:ax "Completed"   :b 3 :c "Tuesday" :d 9}])

;; (write-csv "./results.csv" data :columns [:ax :b])



(defn write-csv-from-channel [path channel & {:keys [columns]}]
  (spit path  (clojure.string/join ";" (map name columns)))
  (loop [hmap (async/<!! channel)]
    (if (nil? hmap)
      (println "Done writing to " (str path))
      (let [row (clojure.string/join ";" (map json/write-str (map hmap columns)))]
        (spit path (str "\n" row) :append true)
        (recur (async/<!! channel))))))



;;; first implementation of async computation of statistics
;; (def in-path "./res/contexts/files")
;; (def out-path  "./results.csv")
;; (def directory (clojure.java.io/file in-path))
;; (def files (doall (filter #(.isFile %) (file-seq directory))))

;; (def contexts-c (async/chan))
;; (def stats-c (async/chan))

;; (def A (async/thread (doseq [file files] (async/>!! contexts-c (cxt-from-file-to-hashmap file)))))
;; (def B (async/go-loop [] 
;;          (let [val (async/<! contexts-c)] 
;;            (async/>! stats-c (compute-statistics (add-concepts-and-implications-to-hashmap val))))
;;          (recur)))
;; (def C (async/thread
;;          (write-csv-from-channel out-path
;;                                  stats-c 
;;                                  :columns [:name 
;;                                            :generator 
;;                                            :num-concepts 
;;                                            :num-pseudo-intents 
;;                                            :density 
;;                                            :num-distinct-objects
;;                                            :ratio-distinct-objects])))


(defn stats-for-cxt-in-path
  "computes some statistics for all contexts in a given path (includes all subdirectories if path is a directory)"
  [& {:keys [in-path out-path format] :or {out-path "./results.csv"
                                           format :burmeister}}]
  (let [directory (clojure.java.io/file in-path)
        files (filter #(.isFile %) (file-seq directory))
        contexts-c (async/chan)
        stats-c (async/take (count files) (async/chan))]
    (do
      ;; read in all contexts in the path
      (async/thread (doseq [file files] (async/>!! contexts-c (cxt-from-file-to-hashmap file :format format))))
      ;;
      (async/go-loop [] 
        (let [val (async/<! contexts-c)] 
          (async/>! stats-c (compute-statistics val)))
        (recur))
      ;; 
      (async/thread
        (write-csv-from-channel out-path
                                stats-c 
                                :columns [:name 
                                          :generator 
                                          :num-objects
                                          :num-attributes
                                          :num-concepts 
                                          :num-pseudo-intents 
                                          :density 
                                          :num-distinct-objects
                                          :ratio-distinct-objects
                                          :objects-in-concepts-count]))
      (println (str "computing context statistics started for files in " directory)))))


;;; TODO add new function (channel control flow) to imitate a context / all contexts in a folder and save the resulting files
