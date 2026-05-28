(ns mcn.sitemap
  (:require [clj-http.client :as http]
            [clj-http.util :as util]
            [clojure.data.xml :as xml]
            [mcn.util :as mcn-util]))

(defn fetch-sitemap []
  (try
    {:ok (http/get "https://www.motorcyclenews.com/sitemap/zip-files/review.xml.gz"
                   {:headers {"User-Agent" "Mozilla/5.0"}
                    :decompress-body false
                    :as :byte-array})}
    (catch Exception e
      {:err {:type :network-sitemap
             :message (.getMessage e)}})))

(defn parse-sitemap [sitemap]
  (try
    {:ok (-> sitemap
             :body
             util/gunzip
             String.
             mcn-util/strip-bom
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
