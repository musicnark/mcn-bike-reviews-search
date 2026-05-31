(ns mcn.api
  (:require [cheshire.core :as json]
            [clojure.string :as string]
            [ring.middleware.params :refer [wrap-params]]
            [mcn.query :as query]))

(def default-headers
  {"Access-Control-Allow-Headers" "Content-Type"
   "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
   "Access-Control-Allow-Origin" "*"
   "Content-Type" "application/json; charset=utf-8"})

(defn camel-case-keyword [k]
  (let [[head & tail] (string/split (name k) #"-")]
    (keyword
     (str head
          (apply str (map string/capitalize tail))))))

(defn format-response-keys [value]
  (cond
    (map? value)
    (into {}
          (map (fn [[k v]]
                 [(if (keyword? k) (camel-case-keyword k) k)
                  (format-response-keys v)]))
          value)

    (vector? value)
    (mapv format-response-keys value)

    (seq? value)
    (map format-response-keys value)

    :else value))

(defn json-response
  ([body]
   (json-response 200 body))
  ([status body]
   {:status status
    :headers default-headers
    :body (json/generate-string (format-response-keys body))}))

(defn error-response [status type message]
  (json-response status
                 {:error {:type type
                          :message message}}))

(defn parse-json-body [request]
  (try
    (if-let [body (:body request)]
      {:ok (json/parse-stream (clojure.java.io/reader body) keyword)}
      {:err "Request body must be valid JSON."})
    (catch Exception _
      {:err "Request body must be valid JSON."})))

(defn index-handler [state _request]
  (json-response {:name "MCN Bike Reviews Search API"
                  :description "API for searching MCN's bike reviews by the specs of each bike. See API documentation for usage."
                  :endpoints ["/health"  "/api/fields"  "/api/bikes"  "/api/search"]}))

(defn health-handler [state _request]
  (if (:bikes state)
    (json-response {:status "ok"
                    :cache-loaded true})
    (json-response 503
                   {:status "degraded"
                    :cache-loaded false
                    :error (:startup-error state)})))

(defn bike-fields [bikes]
  (->> bikes
       vals
       (keep :ok)
       (mapcat keys)
       set
       sort
       (map name)
       vec))

(defn fields-handler [state _request]
  ;; return searchable fields
  (if-let [bikes (:bikes state)]
    (json-response {:fields (bike-fields bikes)})
    (error-response 503 "cache-not-loaded" "Bike cache has not been loaded.")))

(defn bike-summary [[id result]]
  (when-let [bike (:ok result)]
    {:id id
     :bike-name (:bike-name bike)
     :mcn-rating (:mcn-rating bike)
     :url (:url bike)}))

(defn bike-summaries [bikes]
  (->> bikes
       (keep bike-summary)
       (sort-by :bike-name)
       vec))

(defn paginate [items page per-page]
  (let [page (max 1 page)
        per-page (-> per-page (max 1) (min 100))
        offset (* (dec page) per-page)
        results (vec (take per-page (drop offset items)))]
    {:results results
     :page page
     :per-page per-page
     :count (count results)
     :total (count items)}))

(defn parse-int-or-default [value default]
  (try
    (if value
      (Integer/parseInt value)
      default)
    (catch NumberFormatException _
      default)))

(defn bikes-handler [state request]
  (if-let [bikes (:bikes state)]
    (let [params (:query-params request)
          page (parse-int-or-default (get params "page") 1)
          per-page (parse-int-or-default (get params "per-page") 25)]
      (json-response
       (paginate (bike-summaries bikes) page per-page)))
    (error-response 503 "cache-not-loaded" "Bike cache has not been loaded.")))

(defn bike-detail-handler [state id]
  (if-let [bikes (:bikes state)]
    (if-let [bike (get-in bikes [id :ok])]
      (json-response (assoc bike :id id))
      (error-response 404 "bike-not-found" (str "Bike not found: " id)))
    (error-response 503 "cache-not-loaded" "Bike cache has not been loaded.")))

(def supported-query-types #{"comparison" "and" "or" "not"})
(def supported-operators #{"<" ">" "<=" ">=" "="})
(def supported-sort-directions #{"asc" "desc"})

(defn validate-comparison [{:keys [field op value]} valid-fields]
  (cond
    (not (string? field))
    "Comparison query requires a string field."

    (not (contains? valid-fields field))
    (str "Unknown field: " field)

    (not (contains? supported-operators op))
    (str "Unsupported operator: " op)

    (nil? value)
    "Comparison query requires a value."

    :else nil))

(declare validate-filter)

(defn validate-compound-filter [{:keys [type clauses]} valid-fields]
  (or
   (when-not (contains? supported-query-types type)
     (str "Unsupported query type: " type))
   (when-not (seq clauses)
     (str type " query requires a non-empty clauses array."))
   (some #(validate-filter % valid-fields) clauses)))

(defn validate-not-filter [{:keys [clause]} valid-fields]
  (or
   (when-not clause
     "Not query requires a clause.")
   (validate-filter clause valid-fields)))

(defn validate-filter [filter valid-fields]
  (if-not (map? filter)
    "Filter must be an object."
    (case (:type filter)
      "comparison" (validate-comparison filter valid-fields)
      "and" (validate-compound-filter filter valid-fields)
      "or" (validate-compound-filter filter valid-fields)
      "not" (validate-not-filter filter valid-fields)
      "Query filter requires type: comparison, and, or, or not.")))

(defn validate-sort [{:keys [field direction]} valid-fields]
  (cond
    (not (string? field))
    "Sort requires a string field."

    (not (contains? valid-fields field))
    (str "Unknown sort field: " field)

    (and direction
         (not (contains? supported-sort-directions direction)))
    (str "Unsupported sort direction: " direction)

    :else nil))

(defn validate-search-request [{:keys [filter sort limit]} valid-fields]
  (or
   (when-not filter
     "Search request requires a filter.")
   (validate-filter filter valid-fields)
   (when sort
     (if (map? sort)
       (validate-sort sort valid-fields)
       "Sort must be an object."))
   (when (and limit
              (not (pos-int? limit)))
     "Limit must be a positive integer.")))

(defn search-handler [state request]
  (let [parsed-body (parse-json-body request)
        query-request (:ok parsed-body)
        bikes (:bikes state)
        valid-fields (some-> bikes bike-fields set)]
    (cond
      (nil? bikes)
      (error-response 503 "cache-not-loaded" "Bike cache has not been loaded.")

      (:err parsed-body)
      (error-response 400 "invalid-json" (:err parsed-body))

      :else
      (if-let [validation-error (validate-search-request query-request valid-fields)]
        (error-response 400 "invalid-query" validation-error)
        (json-response (:ok (query/query-bikes bikes query-request)))))))

(defn not-found-handler [_request]
  (error-response 404 "not-found" "Route not found."))

(defn options-handler [_request]
  {:status 204
   :headers default-headers
   :body ""})

(defn bike-id-from-uri [uri]
  (let [prefix "/api/bikes/"]
    (when (string/starts-with? uri prefix)
      (subs uri (count prefix)))))

(defn app [state]
  (wrap-params
   (fn [request]
     (let [method (:request-method request)
           uri (:uri request)]
       (cond
         (= method :options)
         (options-handler request)

         (= [method uri] [:get "/"])
         (index-handler state request)

         (= [method uri] [:get "/health"])
         (health-handler state request)

         (= [method uri] [:get "/api/fields"])
         (fields-handler state request)

         (= [method uri] [:get "/api/bikes"])
         (bikes-handler state request)

         (= [method uri] [:post "/api/search"])
         (search-handler state request)

         (and (= method :get) (bike-id-from-uri uri))
         (bike-detail-handler state (bike-id-from-uri uri))

         :else
         (not-found-handler request))))))
