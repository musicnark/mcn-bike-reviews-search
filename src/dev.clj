(ns dev
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io]))

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

(->> [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z]
     (map 
      (fn [c]
        (-> c
            int
            inc
            char
            )))
     (map int)
     (filter even?)
     (map char))

(->> [2 4 3 1 5 7 6 8 9 0]
     sort
     (filter even?)
     (reduce +))


(->> [1 2 3 4 5 6 7]
     (map inc)
     (filter even?))

(let [x [0 1 2 3 4]]
  (map #(- 1 %) x))
