(ns mcn.core
  (:require [clojure.core.async :refer [<!!]]
            [mcn.util :as util]
            [mcn.sitemap :as sitemap]
            [mcn.pipeline :as pipeline]
            [mcn.storage :as storage]
            [mcn.retry :as retry]
            [mcn.query :as query]))

(defn fetch-bikes-map
  "Pipeline that takes the bike specs from web page to local cache, from start to finish."
  [sitemap]
  (let [bikes (-> (util/bind sitemap sitemap/parse-sitemap)
                  (util/bind sitemap/urls-to-fetch)
                  (util/bind pipeline/merge-html-chans)
                  (util/bind pipeline/parse-pipeline)
                  (util/bind pipeline/collect-results))]
    (if (instance? clojure.core.async.impl.channels.ManyToManyChannel bikes)
      (<!! bikes)
      bikes))) ;; TODO add bind support (return {:ok bikes})

(defn get-or-fetch-bikes-map
  "Loads the local cache of bike specs into memory.

  Optionally set the `path` to load from or save to, and whether it should re-fetch the data as `bool`."
  ([]
   (get-or-fetch-bikes-map storage/default-cache-path false #(fetch-bikes-map (sitemap/fetch-sitemap))))
  ([path force-refresh?]
   (get-or-fetch-bikes-map path force-refresh? #(fetch-bikes-map (sitemap/fetch-sitemap))))
  ([path force-refresh? fetch-bikes]
   (if (and (not force-refresh?) (storage/cache-exists? path))
     (storage/load-bikes-map path)
     (let [bikes (fetch-bikes)]
       (if (util/err? bikes)
         bikes
         (let [saved (storage/save-bikes-map! path bikes)]
           (if (util/err? saved)
             saved
             {:ok bikes})))))))

;; Re-export query for convenience
(def query-bikes query/query-bikes)
(def update-bikes-map retry/update-bikes-map)

;; TODO:
;; KEY: [SKIP] = not necessary for SLC version
;; - put name of the bike in the map (test with just one url) [DONE]
;; - add header image url to bike specs? for front-end?
;; - organise code into different files/namespaces~
;; - function doc strings
;; - rewrite parse-bikes to ensure pair mismatch is not possible (see example in dev/dev.clj)
;; - retry for any bikes returning :err
;; - add bike review url as field in map [DONE]
;; - add owners reviews rating as field in map
;; - add in-copy scores as a field in map (reliability, looks, suspension, engine, etc) [SKIP]
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
;; - include tests + standardised testing framework
;; - add CI/CD pipelines
;; - add accumulated logging/log-centric error handling
;;   - basically turn every println into a redirect to logs~
;; - add documentation strings to functions
;; - *organise functions into different namespaces*
