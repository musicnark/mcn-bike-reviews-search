(ns mcn.api-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [mcn.api :as api]))

(def test-bikes
  {"bike-a" {:ok {:bike-name "bike-a"
                  :seat-height "780mm"
                  :used-price "£3,000"
                  :mcn-rating "4"
                  :url "https://example.com/bike-a"}}
   "bike-b" {:ok {:bike-name "bike-b"
                  :seat-height "840mm"
                  :used-price "£4,000"
                  :mcn-rating "5"
                  :url "https://example.com/bike-b"}}})

(defn app
  ([] (api/app {:bikes test-bikes}))
  ([state] (api/app state)))

(defn parse-body [response]
  (json/parse-string (:body response) true))

(defn json-input-stream [body]
  (java.io.ByteArrayInputStream.
   (.getBytes (json/generate-string body) "UTF-8")))

(deftest health-handler-test
  (testing "reports healthy when cache is loaded"
    (let [response ((app) {:request-method :get
                           :uri "/health"})]
      (is (= 200 (:status response)))
      (is (= {:status "ok"
              :cacheLoaded true}
             (parse-body response)))))

  (testing "reports degraded when cache failed to load"
    (let [response ((app {:startup-error {:type :cache-miss}})
                    {:request-method :get
                     :uri "/health"})]
      (is (= 503 (:status response)))
      (is (= "degraded" (:status (parse-body response))))
      (is (false? (:cacheLoaded (parse-body response))))
      (is (nil? (:error (parse-body response)))))))

(deftest fields-handler-test
  (let [response ((app) {:request-method :get
                         :uri "/api/fields"})
        body (parse-body response)]
    (is (= 200 (:status response)))
    (is (= ["bike-name" "mcn-rating" "seat-height" "url" "used-price"]
           (:fields body)))))

(deftest bikes-handler-test
  (testing "returns paginated bike summaries"
    (let [response ((app) {:request-method :get
                           :uri "/api/bikes"
                           :query-string "page=1&per-page=1"})
          body (parse-body response)]
      (is (= 200 (:status response)))
      (is (= {:page 1
              :perPage 1
              :count 1
              :total 2}
             (select-keys body [:page :perPage :count :total])))
      (is (= [{:id "bike-a"
               :bikeName "bike-a"
               :mcnRating "4"
               :url "https://example.com/bike-a"}]
             (:results body))))))

(deftest bike-detail-handler-test
  (testing "returns full details for one bike"
    (let [response ((app) {:request-method :get
                           :uri "/api/bikes/bike-a"})
          body (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "bike-a" (:id body)))
      (is (= "780mm" (:seatHeight body)))))

  (testing "returns 404 for an unknown bike"
    (let [response ((app) {:request-method :get
                           :uri "/api/bikes/missing-bike"})]
      (is (= 404 (:status response)))
      (is (= "bike-not-found" (get-in (parse-body response) [:error :type]))))))

(deftest search-handler-test
  (testing "returns query results for a valid search"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (json-input-stream
                                  {:filter {:type "comparison"
                                            :field "seat-height"
                                            :op "<"
                                            :value 800}
                                   :limit 1})})
          body (parse-body response)]
      (is (= 200 (:status response)))
      (is (= 1 (:count body)))
      (is (= "bike-a" (-> body :results first :bikeName)))))

  (testing "applies default limit when omitted"
    (let [many-bikes (into {}
                           (for [n (range 30)]
                             [(str "bike-" n)
                              {:ok {:bike-name (str "bike-" n)
                                    :seat-height "780mm"
                                    :url (str "https://example.com/bike-" n)}}]))
          response ((app {:bikes many-bikes})
                    {:request-method :post
                     :uri "/api/search"
                     :body (json-input-stream
                            {:filter {:type "comparison"
                                      :field "seat-height"
                                      :op "<"
                                      :value 800}})})
          body (parse-body response)]
      (is (= 200 (:status response)))
      (is (= 25 (:count body)))
      (is (= 30 (:totalMatches body)))))

  (testing "caps large limits"
    (let [many-bikes (into {}
                           (for [n (range 120)]
                             [(str "bike-" n)
                              {:ok {:bike-name (str "bike-" n)
                                    :seat-height "780mm"
                                    :url (str "https://example.com/bike-" n)}}]))
          response ((app {:bikes many-bikes})
                    {:request-method :post
                     :uri "/api/search"
                     :body (json-input-stream
                            {:filter {:type "comparison"
                                      :field "seat-height"
                                      :op "<"
                                      :value 800}
                             :limit 1000})})
          body (parse-body response)]
      (is (= 200 (:status response)))
      (is (= 100 (:count body)))
      (is (= 120 (:totalMatches body)))))

  (testing "rejects invalid JSON"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes "not-json" "UTF-8"))})]
      (is (= 400 (:status response)))
      (is (= "invalid-json" (get-in (parse-body response) [:error :type])))))

  (testing "rejects oversized JSON bodies"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes (apply str (repeat 70000 "x")) "UTF-8"))})]
      (is (= 413 (:status response)))
      (is (= "body-too-large" (get-in (parse-body response) [:error :type])))))

  (testing "rejects unknown fields"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (json-input-stream
                                  {:filter {:type "comparison"
                                            :field "not-a-field"
                                            :op "<"
                                            :value 800}})})]
      (is (= 400 (:status response)))
      (is (= "invalid-query" (get-in (parse-body response) [:error :type])))))

  (testing "rejects too many compound clauses"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (json-input-stream
                                  {:filter {:type "and"
                                            :clauses (vec
                                                      (repeat 26
                                                              {:type "comparison"
                                                               :field "seat-height"
                                                               :op "<"
                                                               :value 800}))}})})]
      (is (= 400 (:status response)))
      (is (= "invalid-query" (get-in (parse-body response) [:error :type])))))

  (testing "rejects overly nested queries"
    (let [nested-filter (reduce
                         (fn [filter _]
                           {:type "not"
                            :clause filter})
                         {:type "comparison"
                          :field "seat-height"
                          :op "<"
                          :value 800}
                         (range 11))
          response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (json-input-stream {:filter nested-filter})})]
      (is (= 400 (:status response)))
      (is (= "invalid-query" (get-in (parse-body response) [:error :type])))))

  (testing "rejects unsupported operators"
    (let [response ((app) {:request-method :post
                           :uri "/api/search"
                           :body (json-input-stream
                                  {:filter {:type "comparison"
                                            :field "seat-height"
                                            :op "contains"
                                            :value 800}})})]
      (is (= 400 (:status response)))
      (is (= "invalid-query" (get-in (parse-body response) [:error :type]))))))

(deftest cors-test
  (let [response ((app) {:request-method :options
                         :uri "/api/search"})]
    (is (= 204 (:status response)))
    (is (= "*" (get-in response [:headers "Access-Control-Allow-Origin"])))))
