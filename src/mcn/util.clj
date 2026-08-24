(ns mcn.util
  (:require [clojure.string :as string]))

(defn clean-keyword [s]
  (-> s
      string/trim
      (string/replace #":" "")
      string/lower-case
      (string/replace #" " "-")
      (string/replace #"1/4" "quarter")
      (string/replace #"/" "-")
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

;; TODO improve parsing logic
(defn clean-bike-name [s]
  (when (url? s)
    (-> s
        (string/split #"bike-reviews/")
        second
        (string/replace #"/" " ")
        string/trim
        (string/replace #" " "-"))))

(defn first-token [s]
  (let [i (.indexOf s " ")]
    (if (neg? i)
      s
      (subs s 0 i))))

(defn ok? [res] (contains? res :ok))
(defn err? [res] (contains? res :err))

(defn bind
  "Binds the the value inside `{:ok value}` to the function `f`, or propagates an error of this shape:
  `{:err {:type :foo
          :message \"bar\"}}`"
  [res f]
  (if (ok? res)
    (f (:ok res))
    res))



(defn normalize-display-value
  "Trim strings, map unavailable markers like \"-\" to nil, and drop obvious HTML debris."
  [v]
  (cond
    (nil? v) nil
    (string? v)
    (let [s (string/trim v)]
      (cond
        (or (empty? s) (= s "-") (= s "—") (= s "N/A") (= s "n/a")) nil
        (re-find #"(?i)<[^>]+>" s) (let [cleaned (-> s
                                                     (string/replace #"(?i)<[^>]+>" "")
                                                     string/trim)]
                                     (when-not (or (empty? cleaned) (= cleaned "-")) cleaned))
        :else s))
    (map? v) (into {} (map (fn [[k val]] [k (normalize-display-value val)]) v))
    (vector? v) (mapv normalize-display-value v)
    (seq? v) (map normalize-display-value v)
    :else v))
