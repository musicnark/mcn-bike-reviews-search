(ns dev
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :as async :refer [go-loop go thread <! >! <!! >!! chan take merge timeout pipeline]])
  (:require [core :as mcn])
  (:require [clj-http.conn-mgr :as conn]))

;; (add-tap (fn [x] (spit "src/log.txt" (pr-str x) :append true)))

(defn bind [res f]
  (if (mcn/ok? res)
    (f (:ok res))
    res))

(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

;; parse csv for each second column (url), store in list
(defn valid-url? [url]
  (and (string? url)
       (seq url)
       (> (count url) 5)  ; minimum reasonable length
       (and (.contains url ".")
           (.startsWith url "http"))))

(def input-table
  (try
  {:ok (with-open [reader (io/reader "src/Bike_Reviews.csv")]
    (doall
     (csv/read-csv reader)))}
  (catch Exception e
    {:err {:type :file
           :message (.getMessage e)}})))

(defn parse-urls [table]
  {:ok
   (->> table
        (map second)
        (filter valid-url?)
        (vec))})

(def urls (-> input-table
     (bind parse-urls)))

(def urls-to-fetch
  (-> input-table
      (bind parse-urls)
      (:ok)))

;; ASYNC
;; 'fan-in'
(defn merge-html-chans [urls-to-fetch]
  (async/merge (doall (map mcn/fetch-bikes-async urls-to-fetch))))

(defn parse-pipeline [input-chan num-workers]
  (let [output-chan (chan)]
    (pipeline num-workers output-chan (map parse-bike) input-chan)
    output-chan))

(defn collect-results [parsed-chan]
  (go-loop [results {}]
    (let [result (<! parsed-chan)]
      (if result
        (let [key (or (get-in result [:ok :bike-name])
                      (keyword (str "bike-" (count results))))
              ;; TODO: Track failed fetches here (when (:err result))
              ]
          (recur (assoc results key result)))
        results))))

(defn results [urls-to-fetch]
  (-> urls-to-fetch
      merge-html-chans
      (parse-pipeline 15)
      collect-results
      <!!))

;; (<!! (collect-results (parse-pipeline (merged-chans urls-to-fetch) 5)))


;; (go-loop [results {}]
;;   (let [result (<! parsed-chan)]
;;     (if result
;;       (let [key (or (get-in result [:ok :bike-name])
;;                     (keyword (str "bike-" (count results))))]
;;         (recur (assoc results key result)))
;;       results)))  ; returns final map when channel closes

;; TODO is this still needed or?
(defn fan-out [merged-chans]
  (go-loop [res []] ;; outer loop (to restart batch when it's finished)
    (let [batch (loop [n 50
                       batch []]
                  (if (zero? n)
                    batch
                    (if-let [val (<! merged-chans)]
                      (do
                        (println "Val: " (str (clojure.core/take 100 val)))
                        (if (mcn/ok? val) ;; TODO ignores failed fetches, but need putting into a 'failed' queue for later retries
                          (recur (dec n) (conj batch val))
                          (recur (dec n) batch)))
                      (reduced batch))))
          batch (if (reduced? batch) @batch batch)]
      (if (seq batch)
        (do
          (<! (timeout 500))
          (println "^^ Batch ^^")
          ;; maybe here is where you parse the bikes? can it be done asynchronously too?
          (recur (into res batch)))
        res))))

;; (println "Final result:" result)

;; TODO rewrite to take directly from channels while fetching urls
  ;; (def maps (pmap parse-bike result))

;; /ASYNC

(defn parse-bike [response-chan]
  (let [doc (html/html-snippet (:body (:ok response-chan)))
        ;; all elements in "Facts & Figures" tables
        facts-figures-labels (map #(mcn/clean-keyword (apply str (:content %)))
                                  (html/select doc [:.review__facts-and-figures__item__label]))
        facts-figures-values (map #(apply str (:content %)) ;; TODO apply filtered-str? that parses and filters out HTML gubbins?
                                  (html/select doc [:.review__facts-and-figures__item__value]))
        ;; MCN Star Rating (separate from other facts/figures)
        mcn-star-rating-label [:mcn-rating]
        mcn-star-rating-value [(some-> (html/select doc [:.star-rating__stars])
                                       first
                                       :attrs
                                       :title
                                       mcn/first-token)]
        bike-name-label [:bike-name]
        bike-name-value [(some-> doc
                                (html/select [[:link (html/attr= :rel "canonical")]]) ;; TODO make each of these parsing functions async?
                                first
                                :attrs
                                :href
                                mcn/clean-bike-name)]

        ;; all-data (into {} (concat facts-figures-labels))
        ]

    ;; wrap return val:
    (println "parsed: " bike-name-value)
      (if (and (seq facts-figures-labels) (seq facts-figures-values) (not (nil? mcn-star-rating-value)))
        {:ok (zipmap
              (concat facts-figures-labels mcn-star-rating-label bike-name-label)   ;; <- these *should* always be equal lengths
              (concat facts-figures-values mcn-star-rating-value bike-name-value))} ;; <-
        {:err {:type :parse
                 :message "labels or values weren't found in HTML response."}})))


;;  ;; :async? in options map need to be true
;; (http/get "https://google.com"
;;             {:async? true}
;;             ;; respond callback
;;             (fn [response] (println "response is:" response))
;;             ;; raise callback
;;             (fn [exception] (println "exception message is: " (.getMessage exception)))) 



;; ;; Inspiration for parse-bike rewrite
;; (defn parse-bike [response]
;;   (let [doc (html/html-snippet (:body (:ok response)))
;;         facts-figures-pairs (for [label (html/select doc [:.review__facts-and-figures__item__label])
;;                                    value (html/select doc [:.review__facts-and-figures__item__value])
;;                                    :when (= (:content label) (:content (html/select doc [:.review__facts-and-figures__item__value])))]
;;                                [(clean-keyword (apply str (:content label)))
;;                                 (apply str (:content value))])
        
;;         mcn-rating (-> (html/select doc [:.star-rating__stars])
;;                        first
;;                        :attrs
;;                        :title
;;                        first-token)
        
;;         bike-name (-> (html/select doc [[:link (html/attr= :rel "canonical")]])
;;                      first
;;                      :attrs
;;                      :href
;;                      clean-bike-name)
;;         all-data (into {} (concat facts-figures-pairs
;;                                   [[:mcn-rating mcn-rating]
;;                                    [:bike-name bike-name]]))]
;;     (if (and (seq facts-figures-pairs) mcn-rating bike-name)
;;       {:ok all-data}
;;       {:err {:type :parse
;;              :message "Required fields missing in HTML response."}})));


(defn merge-html-chans [urls-to-fetch]
  (async/merge (doall (map mcn/fetch-bikes-async urls-to-fetch))))

(defn parse-pipeline [input-chan num-workers]
  (let [output-chan (chan)]
    (pipeline num-workers output-chan (map parse-bike) input-chan)
    output-chan))

(defn collect-results [parsed-chan]
  (go-loop [results {}]
    (let [result (<! parsed-chan)]
      (if result
        (let [key (or (get-in result [:ok :bike-name])
                      (keyword (str "bike-" (count results))))
              ;; TODO: Track failed fetches here (when (:err result))
              ]
          (recur (assoc results key result)))
        results))))

(defn results [urls-to-fetch]
  (-> urls-to-fetch
      merge-html-chans
      (parse-pipeline 15)
      collect-results
      <!!))

;; TODO
;; - how to batch async jobs with timeout [DONE]
;; - *how to parse results as they come back asynchronously?*
;; - handling for duplicate bikes
;; - write a sync version of the code to test against?
;; - integrate rate limiting and error handling into async pipeline
