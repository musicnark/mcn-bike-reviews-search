(ns dev
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string]))

(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

(def doc
  (html/html-snippet (:body (http/get url
                      {:headers {"User-Agent" "Mozilla/5.0"}}))))

(defn get-star-rating [url]
  (let [doc (html/html-snippet (:body (http/get url
                      {:headers {"User-Agent" "Mozilla/5.0"}})))]
(:title (:attrs (first (html/select doc [:.star-rating__stars]))))
    ))
