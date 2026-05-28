(ns mcn.parse
  (:require [net.cgrand.enlive-html :as html]
            [mcn.util :as mcn-util]))

(defn parse-bike [result]
  (let [url (get-in result [:ok :url])
        response (get-in result [:ok :response])
        doc (html/html-snippet (:body response))
        ;; select all elements in "Facts & Figures" tables
        facts-figures-labels (map #(mcn-util/clean-keyword (apply str (:content %)))
                                  (html/select doc [:.review__facts-and-figures__item__label]))
        facts-figures-values (map #(apply str (:content %)) ;; TODO apply filtered-str? that parses and filters out HTML gubbins?
                                  (html/select doc [:.review__facts-and-figures__item__value]))
        ;; MCN Star Rating (separate from other facts/figures)
        mcn-star-rating-label [:mcn-rating]
        mcn-star-rating-value [(some-> (html/select doc [:.star-rating__stars])
                                       first
                                       :attrs
                                       :title
                                       mcn-util/first-token)]
        bike-url-label [:url]
        bike-url-value [(some-> doc
                                (html/select [[:link (html/attr= :rel "canonical")]])
                                first
                                :attrs
                                :href)]
        bike-name-label [:bike-name]
        bike-name-value [(mcn-util/clean-bike-name (first bike-url-value))]
        ;; TODO all-data (into {} (concat facts-figures-labels))
        ]

    (println "parsed: " bike-name-value)
    (if (and (seq facts-figures-labels) (seq facts-figures-values) (not (nil? mcn-star-rating-value)))
      {:ok (zipmap
            (concat facts-figures-labels mcn-star-rating-label bike-url-label bike-name-label)
            (concat facts-figures-values mcn-star-rating-value bike-url-value bike-name-value))}
      {:err {:type :parse-html
             :url url
             :message "labels or values weren't found in HTML response."}})))
