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

(def known-json-keys
  {"clause" :clause
   "clauses" :clauses
   "direction" :direction
   "field" :field
   "filter" :filter
   "limit" :limit
   "op" :op
   "sort" :sort
   "type" :type
   "value" :value})

(def max-json-body-chars 65536)
(def default-search-limit 25)
(def max-search-limit 100)
(def max-query-depth 10)
(def max-query-clauses 25)

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

(defn read-limited-body [body]
  (let [reader (clojure.java.io/reader body)
        buffer (char-array 4096)]
    (loop [chunks []
           total 0]
      (let [read-count (.read reader buffer)]
        (if (= -1 read-count)
          (apply str chunks)
          (let [new-total (+ total read-count)]
            (if (> new-total max-json-body-chars)
              (throw (ex-info "Request body is too large." {:type :body-too-large}))
              (recur (conj chunks (String. buffer 0 read-count))
                     new-total))))))))

(defn normalize-request-keys [value]
  (cond
    (map? value)
    (into {}
          (map (fn [[k v]]
                 [(get known-json-keys k k)
                  (normalize-request-keys v)]))
          value)
    
    (vector? value)
    (mapv normalize-request-keys value)
    
    :else value))

(defn parse-json-body [request]
  (try
    (if-let [body (:body request)]
      {:ok (-> body
               read-limited-body
               json/parse-string
               normalize-request-keys)}
      {:err "Request body must be valid JSON."})
    (catch clojure.lang.ExceptionInfo e
      (if (= :body-too-large (-> e ex-data :type))
        {:err "Request body is too large." :type :body-too-large}
        {:err "Request body must be valid JSON."}))
    (catch Exception _
      {:err "Request body must be valid JSON."})))

(defn index-handler [state]
  (when-let [bikes (:bikes state)]
    (let [bike-count (->> bikes
                          vals
                          (keep :ok)
                          count)]
      (json-response {:name "MCN Bike Reviews Search API"
                      :description "API for searching MCN's bike reviews by the specs of each bike. See API documentation for usage."
                      :bike-count bike-count
                      :endpoints ["/health"  "/api/fields"  "/api/bikes"  "/api/bikes/random" "/api/search"]}))))

(defn health-handler [state]
  (if (:bikes state)
    (json-response {:status "ok"
                    :cache-loaded true})
    (json-response 503
                   {:status "degraded"
                    :cache-loaded false})))

(defn bike-fields [bikes]
  (->> bikes
       vals
       (keep :ok)
       (mapcat keys)
       set
       sort
       (map name)
       vec))

(defn fields-handler [state]
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

(defn validate-compound-filter [{:keys [type clauses]} valid-fields depth]
  (or
   (when-not (contains? supported-query-types type)
     (str "Unsupported query type: " type))
   (when-not (vector? clauses)
     (str type " query requires clauses to be an array."))
   (when-not (seq clauses)
     (str type " query requires a non-empty clauses array."))
   (when (> (count clauses) max-query-clauses)
     (str type " query supports at most " max-query-clauses " clauses."))
   (some #(validate-filter % valid-fields (inc depth)) clauses)))

(defn validate-not-filter [{:keys [clause]} valid-fields depth]
  (or
   (when-not clause
     "Not query requires a clause.")
   (validate-filter clause valid-fields (inc depth))))

(defn validate-filter
  ([filter valid-fields]
   (validate-filter filter valid-fields 1))
  ([filter valid-fields depth]
   (cond
     (> depth max-query-depth)
     (str "Query nesting supports at most " max-query-depth " levels.")
     
     (not (map? filter))
     "Filter must be an object."
     
     :else
     (case (:type filter)
       "comparison" (validate-comparison filter valid-fields)
       "and" (validate-compound-filter filter valid-fields depth)
       "or" (validate-compound-filter filter valid-fields depth)
       "not" (validate-not-filter filter valid-fields depth)
       "Query filter requires type: comparison, and, or, or not."))))

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

(defn apply-search-defaults [{:keys [limit] :as request}]
  (assoc request :limit (min (or limit default-search-limit)
                             max-search-limit)))

(defn search-handler [state request]
  (let [parsed-body (parse-json-body request)
        query-request (:ok parsed-body)
        bikes (:bikes state)
        valid-fields (some-> bikes bike-fields set)]
    (cond
      (nil? bikes)
      (error-response 503 "cache-not-loaded" "Bike cache has not been loaded.")
      
      (:err parsed-body)
      (if (= :body-too-large (:type parsed-body))
        (error-response 413 "body-too-large" (:err parsed-body))
        (error-response 400 "invalid-json" (:err parsed-body)))
      
      :else
      (if-let [validation-error (validate-search-request query-request valid-fields)]
        (error-response 400 "invalid-query" validation-error)
        (json-response (:ok (query/query-bikes bikes (apply-search-defaults query-request))))))))

(defn random-bike-handler [state]
  (if-let [bikes (:bikes state)]
    (let [ok-bikes (->> bikes
                        vals
                        (keep :ok)
                        vec)]
      (if (seq ok-bikes)
        (json-response (rand-nth ok-bikes))
        (error-response 404 "bike-not-found" "No bikes are available")))
    (error-response 503 "cache-not-loaded" "Bike cache has not been loaded")))

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
         
         (= [method uri] [:get "/api"])
         (index-handler state)
         
         (= [method uri] [:get "/api/health"])
         (health-handler state)
         
         (= [method uri] [:get "/api/fields"])
         (fields-handler state)
         
         (= [method uri] [:get "/api/bikes"])
         (bikes-handler state request)
         
         (= [method uri] [:get "/api/bikes/random"])
         (random-bike-handler state)

         (= [method uri] [:post "/api/search"])
         (search-handler state request)
         
         (and (= method :get) (bike-id-from-uri uri))
         (bike-detail-handler state (bike-id-from-uri uri))
         
         :else
         (not-found-handler request))))))
