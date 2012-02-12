(ns zetta.tests.json
  (:use clojure.test)

  (:use zetta.core
        zetta.json))

(deftest test-js-string
  (let [result (parse-once js-string "\"hello world\"")]
    (is (done? result))
    (is (= "hello world" (:result result)))))

(deftest test-js-array
  (let [result (parse-once js-array "[\"hello\",
                                      [32, 42],
                                      43,
                                      \"other\"]")]
    (is (done? result))
    (is (= ["hello" [32 42] 43 "other"] (:result result)))))

(deftest test-js-object
  (let [result (parse-once js-object "{ \"name\": \"John Doe\",
                                        \"professional\": false,
                                        \"background\": null,
                                        \"age\": 43,
                                        \"phones\": [\"555-5555\",
                                                     \"666-6666\"],
                                        \"address\": {
                                          \"suite\": \"3241\",
                                          \"street\":
                                            \"90210 \\\"Beverly\\\" Hills\",
                                          \"city\": \"Los Angeles\",
                                          \"state\": \"California\",
                                          \"country\": \"USA\"}}")]
    (is (done? result))
    (is (= { :name "John Doe"
             :age 43
             :professional false
             :background nil
             :phones ["555-5555" "666-6666"]
             :address {
               :suite "3241"
               :street "90210 \"Beverly\" Hills"
               :city "Los Angeles"
               :state "California"
               :country "USA"}}))))

