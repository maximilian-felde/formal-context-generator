(ns experiments.context-imitations
  (:require [conexp.fca.random-contexts :as rd]
            [conexp.io.contexts :as cxtio]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [experiments.context-statistics :refer [compute-statistics cxt-from-file-to-hashmap]]))


#_(def directory (clojure.java.io/file "./res/test"))
#_(def files (filter #(.isFile %) (file-seq directory)))

(defn files-to-in-c 
  [in-dir format in-c] 
  (let [directory (clojure.java.io/file in-dir)
        files (filter #(.isFile %) (file-seq directory))]
    (async/thread (do (doseq [file files] (async/>!! in-c (cxt-from-file-to-hashmap file :format format)))
                      (async/close! in-c)))))

(defn imitate-context 
  "imitate context and put result in new hashmap"
  [&{:keys [hm model id] :or {model "dirichlet"}}]
  (let [imitated-cxt (cond 
           (= model "dirichlet") 
           (rd/imitate-context-with-dirichlet (:cxt hm))
           (= model "cointoss")
           (rd/imitate-context-with-cointoss (:cxt hm))
           (= model "resampling")
           (rd/imitate-context-with-resampling (:cxt hm))
           (= model "categorical")
           (rd/imitate-context-with-categorical (:cxt hm))
           )]
    (->>
     {:name (str model "/" (clojure.string/replace (str (:name hm)) "." "-") "-imitated-" model "-" id ".cxt")
      :generator model
      :group-id (str (:name hm))
      :cxt imitated-cxt}
     )))


(defn do-stuff
  [& {:keys [N models in-c out-c ts]}]
  (let [compute-for-original (fn compute-for-original [hm]
                               (assoc hm 
                                      :generator "original" 
                                      :name (str "original/" (:name hm))
                                      :group-id (str (:name hm))))
        imitatefns (for [i (range N)
                         model models] 
                     (partial imitate-context :model model :id i :hm)) 
        fns (map #(comp compute-statistics %) (conj imitatefns compute-for-original))
        xf (mapcat (apply juxt fns))]
    (async/pipeline ts out-c xf in-c))
  )

#_(defn do-stuff-async-pipeline
  [& {:keys [N models in-c out-c]}]
  (let [compute-for-original (fn compute-for-original [hm]
                               (assoc hm 
                                      :generator "original" 
                                      :name (str "original/" (:name hm))
                                      :group-id (str (:name hm))))
        af (fn [hm result]
             (async/go
              (async/>! result (compute-statistics (assoc hm 
                                                         :generator "original" 
                                                         :name (str "original/" (:name hm))
                                                         :group-id (str (:name hm)))))
              (doseq [n (range N)
                      model models]
                (async/>! result (compute-statistics (imitate-context :hm hm :id n :model model))))
              (async/close! result)))]
    (async/pipeline-async 4 out-c af in-c)))

#_(defn do-stuff
 ""
  [num-imitations]
  (let [N num-imitations]
    (loop [hm (async/<!! in-c)]
      (if (nil? hm)
        :done-processing-input 
        (do (async/go
              (async/>! out-c (compute-statistics (assoc hm 
                                                         :generator "original" 
                                                         :name (str "original/" (:name hm))
                                                         :group-id (str (:name hm)))))
              (doseq [n (range N)]
                (do
                  (async/>! out-c (imitate-context :hm hm :id n :model "dirichlet"))
                  (async/>! out-c (imitate-context :hm hm :id n :model "categorical"))
                  (async/>! out-c (imitate-context :hm hm :id n :model "resampling"))
                  (async/>! out-c (imitate-context :hm hm :id n :model "cointoss")))))            
            (recur (async/<!! in-c)))))))


(defn main-computations
 ""
  [& {:keys [N models in-c out-c]}]
  (loop [hm (async/<!! in-c)]
    (if (nil? hm)
      (do
        (println :done-processing-input)
        :done-processing-input) 
      (do 
        (println "start processing " (:name hm))
        (async/go
            (async/>! out-c (compute-statistics (assoc hm 
                                                       :generator "original" 
                                                       :name (str "original/" (:name hm))
                                                       :group-id (str (:name hm)))))
            (doseq [n (range N)
                    model models]
              #_(println n model)
              (async/>! out-c (async/<! (compute-statistics (imitate-context :hm hm :id n :model model))))))    
          (recur (async/<!! in-c))))))



(defn main-computations-parallel
  ""
  [& {:keys [N models in-c mid-c out-c ts]}]
  (do
    (async/pipeline-blocking ts out-c (map compute-statistics) mid-c)
    (loop [hm (async/<!! in-c)]
      (if (nil? hm)
        (do
          (println "finished handing off input to further processing")
          :done-processing-input) 
        (do 
          (println "start processing " (:name hm))
          (async/>!! mid-c (assoc hm 
                            :generator "original" 
                            :name (str "original/" (:name hm))
                            :group-id (str (:name hm))))
          (doseq [n (range N)
                  model models]
            (async/>!! mid-c (imitate-context :hm hm :id n :model model)))          
          (recur (async/<!! in-c)))))
    ))


(defn write-context-and-result [out-c out-dir & {:keys [columns]
                                           :or {columns [:name 
                                                         :generator
                                                         :group-id
                                                         :num-objects
                                                         :num-attributes
                                                         :num-concepts 
                                                         :num-pseudo-intents 
                                                         :density 
                                                         :num-distinct-objects
                                                         :ratio-distinct-objects
                                                         :objects-in-concepts-count
                                                         :shannon-entropy
                                                         :entropy
                                                         :column-sum-distribution
                                                         :row-sum-distribution
                                                         :object-hull-size-distribution
                                                         ]}}]
  (io/make-parents (str out-dir "/filename"))
  (let [dir (str out-dir (if (clojure.string/ends-with? out-dir "/") "" "/"))
        results-file (str dir "results.csv")]
    (spit results-file  (clojure.string/join ";" (map name columns)))
    (loop [hmap (async/<!! out-c)]
      (if (nil? hmap)
        (println "done writing contexts and computed statistics to file")
        (do
          (io/make-parents (str dir (:name hmap)))
          (cxtio/write-context :burmeister (:cxt hmap) (str dir (:name hmap)))
          (let [row (clojure.string/join ";" (map json/write-str (map hmap columns)))]
            (spit results-file (str "\n" row) :append true)
            (recur (async/<!! out-c))))))))


(defn process-contexts
  "read contexts from 'in-dir' in 'in-format', imitate each context 'N' times per random generation model,
compute context statistics and write the contexts and statistics to 'out-dir'
compute with 'threads' many threads
"
  ([in-dir in-format N out-dir]
   (process-contexts in-dir in-format N out-dir 4)
   )
  ([in-dir in-format N out-dir ts]
   (let [in-c (async/chan)
         out-c (async/chan 16)
         mid-c (async/chan 8)]
     (do
       (async/thread (files-to-in-c in-dir in-format in-c))
       (async/thread (main-computations-parallel :N N :models ["dirichlet" "cointoss" "resampling"] :in-c in-c :mid-c mid-c :out-c out-c :ts ts))
       #_(do-stuff :N N :models ["dirichlet" "cointoss" "resampling"] :in-c in-c :out-c out-c :ts ts)
       #_(do-stuff-async-pipeline  :N N :models ["dirichlet" "cointoss" "resampling"] :in-c in-c :out-c out-c)
       (write-context-and-result out-c out-dir)
       nil))))


#_(process-contexts "./res/test" :burmeister 5 "./results/")
