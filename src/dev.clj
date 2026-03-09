(ns dev
  (:require [clj-http.client :as http])
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.string :as string]))

;; (add-tap (fn [x] (spit "src/log.txt" (pr-str x) :append true)))

(def url "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")

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
