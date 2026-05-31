(ns mcn.query
  (:require [clojure.string :as string]))

(defn parse-number [value]
  (cond
    (number? value)
    value
    (string? value)
    (let [cleaned (string/replace value #"," "")]
      (when-let [match (re-find #"-?\d+(\.\d+)?" cleaned)]
        (Double/parseDouble (first match))))
    :else
    nil))

(def operators
  {"<" <
   ">" >
   "<=" <=
   ">=" >=
   "=" ==})

(defn compare-field [bike {:keys [field op value]}]
  (let [field-key (keyword field)
        actual-value (get bike field-key)
        actual-number (parse-number actual-value)
        target-number (parse-number value)
        operator (get operators op)]
    (boolean
     (and operator
          actual-number
          target-number
          (operator actual-number target-number)))))

(defn matches? [bike query]
  (case (:type query)
    "comparison"
    (compare-field bike query)

    "and"
    (every? #(matches? bike %) (:clauses query))

    "or"
    (boolean
     (some #(matches? bike %) (:clauses query)))

    "not"
    (not (matches? bike (:clause query)))

    false))

(defn sort-value [bike field]
  (parse-number (get bike (keyword field))))

(defn sort-results [results {:keys [field direction]}]
  (let [sorted (sort-by #(sort-value % field) results)]
    (if (= direction "desc")
      (reverse sorted)
      sorted)))

(defn query-bikes [bikes {:keys [filter sort limit]}]
  (let [matches
        (->> bikes
             (keep (fn [[bike-id result]]
                     (when-let [bike (:ok result)]
                       (when (matches? bike filter)
                         (assoc bike :id bike-id)))))
             vec)

        sorted-results
        (if sort
          (vec (sort-results matches sort))
          matches)

        limited-results
        (if limit
          (vec (take limit sorted-results))
          sorted-results)]

    {:ok {:results limited-results
          :count (count limited-results)
          :total-matches (count matches)}}))

;; TODO
;; - place results with no value (nil) at the end, regardless of sort order
;; - Add query validation before evaluation. For example, reject unknown :type, missing :field, unsupported :op, empty :clauses, and malformed not nodes.
;; - Decide the public response shape for query-bikes, probably {:ok {:results [...] :count n :skipped [...]}}.
;; - Add sorting and limits after filtering, because the API/ frontend will need them quickly.
;; - Add data cleanup in parse-bike, separately from query logic. The messy whitespace and embedded Enlive node strings should be fixed at ingestion, not during querying.
