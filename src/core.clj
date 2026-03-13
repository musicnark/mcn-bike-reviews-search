(ns core
  (:require [clj-http.client :as http])
  (:require [clj-http.util :as util])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :as async :refer [go <! >! <!! chan close!]])
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


(defn strip-bom [s]
  (if (.startsWith s "\uFEFF")
    (subs s 1)
    s))

(defn parse-urls [table]
  {:ok
   (->> table
        (map second)
        (filter url?)
        (vec))})

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

(comment
(clean-bike-name "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/"))

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
;; (def input-table
;;   (try
;;   {:ok (with-open [reader (io/reader "src/Bike_Reviews.csv")]
;;     (doall
;;      (csv/read-csv reader)))}
;;   (catch Exception e
;;     {:err {:type :file
;;            :message (.getMessage e)}})))

(def manifest (try
                {:ok (http/get "https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz"
                       {:headers {"User-Agent" "Mozilla/5.0"}
                        :decompress-body false
                        :as :byte-array
                        })}
                (catch Exception e
                  {:err {:type :network-manifest
                         :message (.getMessage e)}})))

(defn parse-manifest [manifest]
  (try
    {:ok (-> manifest
      :body
      util/gunzip
      String.
      strip-bom
      xml/parse-str
      :content)}
    (catch Exception e
      {:err {:type :parse-manifest
             :message (.getMessage e)}})))

(defn urls-to-fetch [parsed-manifest]
  (let [res (doall
   (map (fn [loc]
          (-> loc
              :content
              first
              :content
              first))
        parsed-manifest))]
    {:ok res}))

(def cm (conn/make-reusable-async-conn-manager
          {:threads 1500              ;; max threads for connecting
           :default-per-route 20   ;; max connections *per host*
           :timeout 10}))

(defn fetch-bikes-async [url]
  (let [ch (chan)]
    (http/get url {:headers {"User-Agent" "Mozilla/5.0"}
                   :async? true
                   :connection-manager cm}
              ;; success callback
              (fn [r]
                (go
                  (>! ch {:ok r})
                  (close! ch)
                  (println "successfully fetched bike")))
              ;; error callback
              (fn [e]
                (go
                  (>! ch {:err {:type :network-page
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
        bike-url-label [:url]
        bike-url-value [(some-> doc ;; TODO rename as bike-url, get bike name from it
                                (html/select [[:link (html/attr= :rel "canonical")]])
                                first
                                :attrs
                                :href
                                )]
        bike-name-label [:bike-name]
        bike-name-value [(clean-bike-name (first bike-url-value))]
        ;; all-data (into {} (concat facts-figures-labels))
        ]

    (println "parsed: " bike-name-value)
    ;; wrap return val:
      (if (and (seq facts-figures-labels) (seq facts-figures-values) (not (nil? mcn-star-rating-value)))
        {:ok (zipmap
              (concat facts-figures-labels mcn-star-rating-label bike-url-label bike-name-label)   ;; <- these *should* always be equal lengths
              (concat facts-figures-values mcn-star-rating-value bike-url-value bike-name-value))} ;; <-
        {:err {:type :parse-html
                 :message "labels or values weren't found in HTML response."}})))

(defn merge-html-chans [urls-to-fetch]
  {:ok (async/merge (doall (map fetch-bikes-async urls-to-fetch)))})

(defn parse-pipeline [input-chan]
  (let [output-chan (chan)]
    (async/pipeline
     15
     output-chan
     (map parse-bike)
     input-chan)
    {:ok output-chan}))

(defn collect-results [parsed-chan]
  (async/go-loop [results {}]
    (let [result (<! parsed-chan)]
      (if result
        (let [key (or (get-in result [:ok :bike-name])
                      (keyword (str "bike-" (count results))))
              ;; TODO: Track failed fetches here (when (:err result))
              ]
          (recur (assoc results key result)))
        {:ok results}))))


;; (def urls-to-fetch
;;   (-> input-table
;;       (bind parse-urls)
;;       (:ok))) ;; TODO proper error handling needed

;; Main
(defn results [manifest]
  (-> (bind manifest parse-manifest)
      (bind urls-to-fetch)
      (bind merge-html-chans)
      (bind parse-pipeline)
      (bind collect-results)
      <!!))

;; TODO:
;; - put name of the bike in the map (test with just one url) [DONE]
;; - rewrite parse-bikes to ensure pair mismatch is not possible (see example in dev.clj)
;; - add bike review url as field in map [DONE]
;; - add owners reviews  as field in map
;; - fetch bike urls from sitemap (https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz)
;;   - compare file hashes to see if it's changed, no update = no fetch operation
;; - fix newlines and tabs included in some strings?
;;   - put logic in to individually parse each inner tag of data 
;; - make it async~
;;   - url fetching [DONE]
;;   - page parsing [DONE]
;; - implement DSL/query language
;; - implement API + docs
;; - include tests
;; - add accumulated logging
;; - add documentation strings to functions
;; - *organise functions into different namespaces*

;; TODO (prototyping):
;; - how to batch async jobs with timeout [DONE]
;; - how to parse results as they come back asynchronously? [DONE]
;; - handling for duplicate bikes
;; - write a sync version of the code to test against?
;; - integrate rate limiting and global error handling (via an atom) into async pipeline

;; TODO Concurrency Tutorial:
;; - threads vs go routines?
;; - generate exercises with dummy json data to internalise the basics (message passing, assigning futures, putting and taking from threads/go routines, watching the speed-up real-time)
