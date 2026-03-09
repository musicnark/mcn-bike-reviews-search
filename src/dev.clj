(ns dev
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :refer [go <! >! chan]]))

;; (add-tap (fn [x] (spit "src/log.txt" (pr-str x) :append true)))

(defn ok? [res] (contains? res :ok))

(defn bind [res f]
  (if (ok? res)
    (f (:ok res))
    res))

(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

;; parse csv for each second column (url), store in list
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
   (map second table)})

(-> input-table
    (bind parse-urls))

(def doc
  (-> (http/get url {:headers {"User-Agent" "Mozilla/5.0"}})
      :body
      (tap>)
      html/html-snippet
      ))

(defn get-star-rating [url]
  (let [doc (html/html-snippet (:body (http/get url
                      {:headers {"User-Agent" "Mozilla/5.0"}})))]
    (-> doc
        (html/select [:.star-rating__stars])
        first
        :attrs
        :title)))


(defn fetch-bikes-async [url]
  (http/get url
            {:headers {"User-Agent" "Mozilla/5.0"}
             :async? true}
            (fn [response]
              {:ok response})
            (fn [exception]
              {:err {:type :network
                     :message (.getMessage exception)}})))

(go
  (let [result (<! (fetch-bike url))]
    (println "Parsed result:" result)))

(defn parse-bike [response]
  (let [doc (html/html-snippet (:body (:ok response)))
        ;; all elements in "Facts & Figures" tables
        facts-figures-labels (map #(clean-keyword (apply str (:content %)))
                                  (html/select doc [:.review__facts-and-figures__item__label]))
        facts-figures-values (map #(apply str (:content %)) ;; TODO apply filtered-str? that parses and filters out HTML gubbins?
                                  (html/select doc [:.review__facts-and-figures__item__value]))
        ;; MCN Star Rating (separate from other facts/figures)
        mcn-star-rating-label [:mcn-rating]
        mcn-star-rating-value [(some-> (html/select doc [:.star-rating__stars])
                                       first
                                       :attrs
                                       :title
                                       first-token)]
        bike-name-label [:bike-name]
        bike-name-value [(some-> doc
                                (html/select [[:link (html/attr= :rel "canonical")]])
                                first
                                :attrs
                                :href
                                clean-bike-name)]

        ;; all-data (into {} (concat facts-figures-labels))
        ]

    ;; wrap return val:
      (if (and (seq facts-figures-labels) (seq facts-figures-values) (not (nil? mcn-star-rating-value)))
        {:ok (zipmap
              (concat facts-figures-labels mcn-star-rating-label bike-name-label)   ;; <- these *should* always be equal lengths
              (concat facts-figures-values mcn-star-rating-value bike-name-value))} ;; <-
        {:err {:type :parse
                 :message "labels or values weren't found in HTML response."}})))

(defn fetch-and-parse-single-bike-async [url]
      (let [result-chan (chan 1)]
        (go
          (let [deferred-result (fetch-bike url)
                final-result @deferred-result] ;; This parks the go block, non-blocking for CPU thread
            (if (:ok final-result)
              (>! result-chan (parse-bike (:ok final-result)))
              (>! result-chan final-result))))
        result-chan))
    ;; Usage:
    (go
      (let [result (<! (fetch-and-parse-single-bike-async url))]
        (println "Parsed result:" result)))

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
