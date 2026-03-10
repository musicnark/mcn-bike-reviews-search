(ns core
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :refer [go <! >! chan close! take!]])
  (:require [clj-http.conn-mgr :as conn]))

;; helpers 
(defn clean-keyword [s]
  (-> s
      string/trim
      (string/replace #":" "")
      string/lower-case
      (string/replace #" " "-")
      keyword))

(defn url? [s]
  (try
    (some? (java.net.URL. s))
    (catch Exception _ false)))

;; TODO improve parsing logic
(defn clean-bike-name [s]
  (when (url? s)
    (-> s
        (string/split #"bike-reviews/")
        second
        (string/replace #"/" " ")
        string/trim
        (string/replace #" " "-")
        )))

(clean-bike-name "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

(defn first-token [s]
  (let [i (.indexOf s " ")]
    (if (neg? i)
      s
      (subs s 0 i))))


(defn ok? [res] (contains? res :ok))
(defn err? [res] (contains? res :err))
;; essentially 'unwrap' or 'match'. think 'then' in pipeline
(defn bind [res f]
  (if (ok? res)
    (f (:ok res))
    res))

(defn map-ok [f]
  (fn [xs]
    {:ok (map f xs)}))

(defn pmap-ok [f]
  (fn [xs]
    {:ok (pmap f xs)}))

;; TODO import + clean up CSV of bikes (or scrape them all from the live site?)
(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

;; basic CSV parsing
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


(defn fetch-bike [url]
  (try
    {:ok (http/get url
                   {:headers {"User-Agent" "Mozilla/5.0"}})}
    (catch Exception e
      {:err {:type :network
             :message (.getMessage e)}})))

(def cm (conn/make-reusable-async-conn-manager
          {:threads 100              ; max threads for connecting
           :default-per-route 100   ; max connections PER HOST
           :timeout 1}))

(defn fetch-bikes-async [url]
  (let [ch (chan)]
    (http/get url {:headers {"User-Agent" "Mozilla/5.0"}
                   :async? true
                   :connection-manager cm}
              ;; success callback
              (fn [r]
                (go
                  (>! ch {:ok r})
                  (close! ch)))
              ;; error callback
              (fn [e]
                (go
                  (>! ch {:err {:type    :network
                                :message (.getMessage e)}})
                  (close! ch))))
    ch))

;; TODO implement in main function
;; (go
;;   (let [result (<! (fetch-bikes-async "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/"))]
;;     (println (parse-bike result))))

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

;; FIXME test
(defn test-query-bike [response]
  ;; await query input from user
  (let [res (:mcn-rating response)] ;; test
    (if (and (string? res) (not (empty? res)))
      {:ok res}
      {:err {:type :query
             :message "key or value not found"}})))

;; ;; Main
;; (-> input-table
;;     (bind parse-urls)
;;     (bind (pmap-ok fetch-bike))
;;     (bind (pmap-ok parse-bike))
;;     (bind (fn [bikes] {:ok (doall bikes)}))
;;     ) ;; FIXME

;; TODO:
;; - put name of the bike in the map (test with just one url)
;; - rewrite parse-bikes to ensure pair mismatch is not possible (see example in dev.clj)
;; - add bike review url as field in map
;; - fix newlines and tabs included in some strings?
;;   - put logic in to individually parse each inner tag of data 
;; - make it async~
;; - implement DSL/query language
;; - implement API + docs
;; - include tests
;; - add accumulated logging
;; - add documentation strings to functions

;; TODO Concurrency Tutorial:
;; - threads vs go routines?
;; - generate exercises with dummy json data to internalise the basics (message passing, assigning futures, putting and taking from threads/go routines, watching the speed-up real-time)
