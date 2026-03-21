;; welcome to the graveyard~
(ns dev
  (:require [clj-http.client :as http])
  (:require [clj-http.util :as util])
  (:require [clojure.string :as string])
  (:require [clojure.data.xml :as xml])
  (:require [core :as mcn]))

;; (add-tap (fn [x] (spit "src/log.txt" (pr-str x) :append true)))


;; DEPRICATED - async version used now
;; (defn fetch-bike [url]
;;   (try
;;     {:ok (http/get url
;;                    {:headers {"User-Agent" "Mozilla/5.0"}})}
;;     (println "Fetched bike")
;;     (catch Exception e
;;       {:err {:type :network
;;              :message (.getMessage e)}})))


;; (go-loop [results {}]
;;   (let [result (<! parsed-chan)]
;;     (if result
;;       (let [key (or (get-in result [:ok :bike-name])
;;                     (keyword (str "bike-" (count results))))]
;;         (recur (assoc results key result)))
;;       results)))  ; returns final map when channel closes


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

;; (defn fan-out [merged-chans]
;;   (go-loop [res []] ;; outer loop (to restart batch when it's finished)
;;     (let [batch (loop [n 50
;;                        batch []]
;;                   (if (zero? n)
;;                     batch
;;                     (if-let [val (<! merged-chans)]
;;                       (do
;;                         (println "Val: " (str (clojure.core/take 100 val)))
;;                         (if (ok? val) ;; TODO ignores failed fetches, but need putting into a 'failed' queue for later retries
;;                           (recur (dec n) (conj batch val))
;;                           (recur (dec n) batch)))
;;                       (reduced batch))))
;;           batch (if (reduced? batch) @batch batch)]
;;       (if (seq batch)
;;         (do
;;           (<! (timeout 500))
;;           (println "^^ Batch ^^")
;;           ;; maybe here is where you parse the bikes? can it be done asynchronously too?
;;           (recur (into res batch)))
;;         res))))


;; TODO add to main code - trim insurance group
;; (string/trim
;;  (->> (-> mcn/rez
;;           (get "suzuki-rv125-van-van-2003")
;;           :ok
;;           :insurance-group)
;;       (re-find #"\d*\ ")))

(defn strip-bom [s]
  (if (.startsWith s "\uFEFF")
    (subs s 1)
    s))

(def res (-> (http/get "https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz"
                       {:headers {"User-Agent" "Mozilla/5.0"}
                        :decompress-body false
                        :as :byte-array
                        })
             :body
             util/gunzip
             String.
             strip-bom
             xml/parse-str
             :content))

(def output
  (map (fn [loc]
         (-> loc
             :content
             first
             :content
             first))
       res))

;; TODO implement in main function?
;; (go
;;   (let [result (<! (fetch-bikes-async "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/"))]
;;     (println (parse-bike result))))

;; Old Main
(comment
 (-> input-table
    (bind parse-urls)
    (bind (pmap-ok fetch-bike))
    (bind (pmap-ok parse-bike))
    (bind (fn [bikes] {:ok (doall bikes)}))
    )) ;; FIXME


;; TODO
;; - how to batch async jobs with timeout [DONE]
;; - how to parse results as they come back asynchronously? [DONE]
;; - handling for duplicate bikes
;; - write a sync version of the code to test against?
;; - integrate rate limiting and global error handling (via an atom) into async pipeline
;; - *organise functions into different namespaces*

;; TODO Outline:
;; - API
;;   - rate limiting + escalating bans
;;   - back-end architecture (web server or nginx?)

;; TODO integrate into main parse/query (formatting the bike price) (also add support for ranges of used prices (just take highest and call it at that?))
(comment
  (def new-prices (->> mcn/rez (map (fn [bike] (when (contains? (second bike) :ok) (-> bike second :ok :new-price))))))
(def used-prices (->> mcn/rez (map (fn [bike] (when (contains? (second bike) :ok) (-> bike second :ok :used-price)))))))

;; TODO add support for price ranges
(defn format-price [s]
  (let [f-str
        (when (some? s)
          (-> s
              (string/replace #"£" "")
              (string/replace #"," "")))]
    (try
      {:ok (Integer/parseUnsignedInt f-str)} ;; TODO filter first, then format - errors not needed for logging, so removes unnecessary try/catch
      (catch Exception e
        {:err {:type :parse-price
               :message (.getMessage e)}}))))

;; TODO
(defn valid-price-str? [s]
  )

(defn +vm [map]
  (when (mcn/ok? map)
    (:ok map)))

(comment
  (->> new-prices
       (map format-price)
       (filter mcn/ok?)
       (map +vm)
       (apply +)))

(comment
  (->> used-prices
       (filter some?)
       (map string/trim)
       (map format-price)
       (filter mcn/ok?)))

(comment
  ;; this should be able to parse a price range too
  (->> used-prices (filter some?) (map string/trim) (map format-price)))


;; prototype query
(filter (fn [val] (and (some? (:max-power (:ok val)))(re-find #"^97 " (:max-power (:ok val))))) (vals rez))

;; reliability rating (run in core ns)
(comment
  (def test-page (html/html-snippet (<!! (fetch-bikes-async "https://www.motorcyclenews.com/bike-reviews/ducati/multistrada-v2s/2022/"))))

  (let [h2 (some-> (html/select test-page [[:h2 (html/attr-contains :class "review__main-content__heading")]]))
        rating (some-> (html/select test-page [[:div (html/attr-contains :class "review__main-content__rating-container")]]))]
    (map vector h2 rating)))

;; (doall (map (fn [v] (when (contains? v :ok) (:ok v)) (map format-price prices))))

;; (map (fn [price] (if (= (first-token price) "£") ( prices)
