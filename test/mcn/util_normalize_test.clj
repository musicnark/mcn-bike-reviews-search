(ns mcn.util-normalize-test
  (:require [clojure.test :refer [deftest is]]
            [mcn.util :as util]))

(deftest normalize-display-value-trims-and-nulls-unavailable
  (is (= "12" (util/normalize-display-value "  12  ")))
  (is (nil? (util/normalize-display-value "-")))
  (is (nil? (util/normalize-display-value "  -  ")))
  (is (= "A" (util/normalize-display-value "<b>A</b>"))))
