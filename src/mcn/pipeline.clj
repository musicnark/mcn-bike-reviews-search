(ns mcn.pipeline
  (:require [clojure.core.async :as async :refer [<! chan]]
            [mcn.fetch :as fetch]
            [mcn.parse :as parse]))

(defn merge-html-chans [urls-to-fetch]
  {:ok (async/merge (doall (map fetch/fetch-bikes-async urls-to-fetch)))}) ;; TODO proper error handling needed

(defn parse-pipeline [input-chan]
  (let [output-chan (chan)]
    (async/pipeline 15
                    output-chan
                    (map parse/parse-bike)
                    input-chan)
    {:ok output-chan})) ;; TODO proper error handling needed

(defn collect-results [parsed-chan]
  (async/go-loop [results {}]
    (let [result (<! parsed-chan)]
      (if result
        (let [key (or (get-in result [:ok :bike-name])
                      (keyword (str "bike-" (count results))))
              ;; TODO: Track failed fetches here (when (:err result))
              ]
          (recur (assoc results key result)))
        results))))
