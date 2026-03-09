(ns core
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io]))

;; helpers 
(defn clean-keyword [s]
  (-> s
      string/trim
      (string/replace #":" "")
      string/lower-case
      (string/replace #" " "-")
      keyword))

;; TODO improve parsing logic
(defn clean-bike-name [s]
  (-> s
      (string/split #"bike-reviews/")
      second
      (string/replace #"/" " ")
      string/trim
      (string/replace #" " "-")
      ))

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
                                clean-bike-name)]]

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

;; Main
(-> input-table
    (bind parse-urls)
    (bind (pmap-ok fetch-bike))
    (bind (map-ok parse-bike))
    (bind (fn [bikes] {:ok (doall bikes)}))
    ) ;; FIXME

;; TODO:
;; - put name of the bike in the map (test with just one url)
;; - fix newlines and tabs included in some strings?
;;   - put logic in to individually parse each inner tag of data 
;; - make it async~
;; - implement DSL/query language
;; - implement API + docs
;; - include tests
;; - add accumulated logging
;; - add documentation strings to functions
