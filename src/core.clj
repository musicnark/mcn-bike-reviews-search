(ns core
  (:require [clj-http.client :as http])
  (:require [clj-http.util :as util])
  (:require [clj-http.conn-mgr :as conn])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.xml :as xml])
  (:require [clojure.core.async :as async :refer [go <! >! <!! chan close!]]))

;; helpers 
(defn clean-keyword [s]
  (-> s
      string/trim
      (string/replace #":" "")
      string/lower-case
      (string/replace #" " "-")
      keyword))

;; TODO make this more robust
(defn url? [s]
  (try
    (some? (java.net.URL. s))
    (catch Exception _ false)))

(defn strip-bom
  "Strips the byte-order mark from the beginning of string `s`, in preparation for XML parsing."
  [s]
  (if (.startsWith s "\uFEFF")
    (subs s 1)
    s))

;; for CSV
;; (defn parse-urls [table]
;;   {:ok
;;    (->> table
;;         (map second)
;;         (filter url?)
;;         (vec))})

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

(comment
  (first-token "8 out of 17")
  (first-token "15 out of 17"))


(defn ok? [res] (contains? res :ok))
(defn err? [res] (contains? res :err))

(defn bind
  "Binds the :ok value of `res` to the function `f`, or propagates the error :err.

  If `res` contains :err, the supplied function is not evaluated.

  Reads well in pipelines (substitute 'bind' for 'then').

  If unfamiliar with the bind pattern, think:

  'unwrap `res` and apply it to `f`, or return :err'"
  [res f]
  (if (ok? res)
    (f (:ok res))
    res))

;; TODO why are these here?
;; (defn map-ok [f]
;;   (fn [xs]
;;     {:ok (map f xs)}))

;; (defn pmap-ok [f]
;;   (fn [xs]
;;     {:ok (pmap f xs)}))

;; basic CSV parsing
;; (def input-table
;;   (try
;;   {:ok (with-open [reader (io/reader "src/Bike_Reviews.csv")]
;;     (doall
;;      (csv/read-csv reader)))}
;;   (catch Exception e
;;     {:err {:type :file
;;            :message (.getMessage e)}})))

(def sitemap
  (try
    {:ok (http/get "https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz"
                   {:headers {"User-Agent" "Mozilla/5.0"}
                    :decompress-body false
                    :as :byte-array
                    })}
    (catch Exception e
      {:err {:type :network-sitemap
             :message (.getMessage e)}})))

(defn parse-sitemap [sitemap]
  (try
    {:ok (-> sitemap
      :body
      util/gunzip
      String.
      strip-bom
      xml/parse-str
      :content)}
    (catch Exception e
      {:err {:type :parse-sitemap
             :message (.getMessage e)}})))

;; TODO can it be done more safely? or with error handling at least?
(defn urls-to-fetch [parsed-sitemap]
  (let [res (doall
   (map (fn [loc]
          (-> loc
              :content
              first
              :content
              first))
        parsed-sitemap))]
    {:ok res}))

(def cm (conn/make-reusable-async-conn-manager
          {:threads 50             ;; max threads for connecting
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
                  (println "ERR: " e) ;; TODO redirect to logging
                  (close! ch))))
    ch))

(defn parse-bike [response]
  (let [doc (html/html-snippet (-> response :ok :body))
        ;; select all elements in "Facts & Figures" tables
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
        ;; TODO all-data (into {} (concat facts-figures-labels))
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
  {:ok (async/merge (doall (map fetch-bikes-async urls-to-fetch)))}) ;; TODO proper error handling needed

(defn parse-pipeline [input-chan]
  (let [output-chan (chan)]
    (async/pipeline
     15
     output-chan
     (map parse-bike)
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

;; Main
(defn get-bikes-map [sitemap]
  (let [bikes (-> (bind sitemap parse-sitemap)
                (bind urls-to-fetch)
                (bind merge-html-chans)
                (bind parse-pipeline)
                (bind collect-results))]

    (if (instance? clojure.core.async.impl.channels.ManyToManyChannel bikes)
      (<!! bikes)
      bikes)))


(comment
  (def rez (get-bikes-map sitemap))
  
  (first rez)
  (get rez "suzuki-burgman-650-2003")
  
  )

;; TODO:
;; - put name of the bike in the map (test with just one url) [DONE]
;; - rewrite parse-bikes to ensure pair mismatch is not possible (see example in dev.clj)
;; - add bike review url as field in map [DONE]
;; - add owners reviews rating as field in map
;; - add in-copy scores as a field in map (reliability, looks, suspension, engine, etc)
;; - add bike model year       as field in map
;; - fetch bike urls from sitemap (https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz) [DONE]
;;   - compare file hashes to see if it's changed, no update = no fetch operation
;; - fix newlines and tabs included in some strings?
;;   - put logic in to individually parse each inner tag of data 
;; - make it async~
;;   - url fetching [DONE]
;;   - page parsing [DONE]
;; - implement DSL/query language
;;   - function takes map/json and searches based on given parameters
;; - implement API + docs
;; - include tests
;; - add CI/CD pipelines
;; - add accumulated logging/log-centric error handling
;;   - basically turn every println into a redirect to logs~
;; - add documentation strings to functions
;; - *organise functions into different namespaces*

;; TODO (prototyping):
;; - how to batch async jobs with timeout [DONE]
;; - how to parse results as they come back asynchronously? [DONE]
;; - handling for duplicate bikes [DONE?]
;; - write a sync version of the code to test against? [DONE?]
;; - integrate rate limiting and global error handling (via an atom) into async pipeline

;; TODO Concurrency Tutorial:
;; - threads vs go routines?
;; - generate exercises with dummy json data to internalise the basics (message passing, assigning futures, putting and taking from threads/go routines, watching the speed-up real-time)
