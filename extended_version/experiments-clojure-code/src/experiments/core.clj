(ns experiments.core
  (:require [experiments.context-imitations :as imit]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))


(def cli-options
  [
   ["-i" "--in-dir INPUT-DIRECTORY-OR-FILE" "input Directory or file"]
   ["-f" "--format CXT-FORMAT" "format of input contexts, e.g., :burmeister" :default :burmeister]
   ["-n" "--number N" "number of imitations per random generator per input context" :default 1]
   ["-o" "--out-dir OUTPUT-DIRECTORY" "output Directory (will be created if not existent)"]
   ["-t" "--threads NUMBER-OF-THREADS" "the number of threads to use in parallel" :default 4 ]
   ])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (if errors 
      (exit 0 summary)
      (let [in-dir (:in-dir options)
            in-format (:format options)
            N (Integer/parseInt (str (:number options)))
            out-dir (:out-dir options)
            ts (Integer/parseInt (str (:threads options)))]
        (if (or (nil? in-dir) (nil? out-dir))
          (exit 0 summary)
          (imit/process-contexts in-dir in-format N out-dir ts))))))
