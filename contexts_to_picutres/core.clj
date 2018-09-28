(ns contexts-to-pictures.core
  (:gen-class)
  (:require [conexp.fca.contexts :refer :all ]
            [conexp.fca.implications :refer :all]
            [conexp.io.contexts :refer :all]
            [clojure.java.io           :as io]
            [clojure.set :refer [subset?]])
  (:import [java.io File]))


(defn get-all-file-paths
  [directory]
  (let [files (remove #(.isDirectory %) (file-seq (File. directory))),
        csv-files (remove #(not (boolean (re-find #".csv" (.getName %)))) files)]
    csv-files)) 


(defn read_csv
  [file]
  (read-context (.getPath file)  :binary-csv))


(defn compute-points
  [context-seq]
  (pmap (fn [ctx] (vector (count (intents ctx)) (count (pseudo-intents ctx)))) context-seq))


(defn concat-point-filename
  [point ctx-file]
  (vector (point 0) (point 1) (str (.getPath ctx-file))))


(defn results-to-file
  [results directory]
  (with-open [wrtr (clojure.java.io/writer directory)]
    (doseq [result results]
      (.write wrtr (str (result 0) ";" (result 1) ";" (result 2) "\n")))))


(defn do-all
  [directory]
  (let [ctx-files (get-all-file-paths directory)
        points (compute-points (map read_csv ctx-files))]
    (results-to-file (map #(concat-point-filename %1 %2) points ctx-files) (str directory "/results"))))


(defn -main [dir & args]
  (if dir (do 
            (println (str dir))
            (time (do-all (str dir)))
            )))
