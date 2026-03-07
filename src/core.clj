(ns core
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string]))

;; helpers 
(defn ok? [res] (contains? res :ok))
(defn err? [res] (contains? res :err))
;; essentially 'unwrap' or 'match'. think 'then' in pipeline
(defn bind [res f]
  (if (ok? res)
    (f (:ok res))
    res))

;; TODO import + clean up CSV of bikes (or scrape them all from the live site?)
(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

(defn fetch-bike [url]
  (try
    {:ok (http/get url
                   {:headers {"User-Agent" "Mozilla/5.0"}})}
    (catch Exception e
      {:err {:type :network
             :message (.getMessage e)}})))

(defn clean-keyword [s]
  (-> s
      string/trim
      (string/replace #":" "")
      string/lower-case
      (string/replace #" " "-")
      keyword))

(defn first-token [s]
  (let [i (.indexOf s " ")]
    (if (neg? i)
      s
      (subs s 0 i))))

(defn parse-bike [response]
  (let [doc (html/html-snippet (:body response))
        ;; all elements in "Facts & Figures" tables
        facts-figures-labels (map #(clean-keyword (apply str (:content %)))
                                        (html/select doc [:.review__facts-and-figures__item__label]))
        facts-figures-values (map #(apply str (:content %))
                                        (html/select doc [:.review__facts-and-figures__item__value]))
        ;; MCN Star Rating (separate from other facts/figures)
        mcn-star-rating-label [:mcn-rating]
        mcn-star-rating-value [(some-> (html/select doc [:.star-rating__stars])
                                first
                                :attrs
                                :title
                                first-token)]]

    ;; wrap return val:
      (if (and (seq facts-figures-labels) (seq facts-figures-values))
        {:ok (zipmap
              (concat facts-figures-labels mcn-star-rating-label)   ;; <- these *should* always be equal lengths
              (concat facts-figures-values mcn-star-rating-value))} ;; <-
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
  (-> (fetch-bike url)
      (bind parse-bike)
      (bind test-query-bike)) ;; FIXME

;; TODO:
;; - import CSV of bikes and map over them
;; - implement DSL/query language
;; - implement API
;; - include tests
;; - add accumulated logging 
;; - make it async~
