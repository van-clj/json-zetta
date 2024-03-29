(ns zetta.tests.json
  (:use clojure.test)

  (:use zetta.core
        zetta.json))

(deftest test-complex-json
  (let [input (slurp "test/fixtures/complex.json")
        result (parse-once json input)]
    (is (done? result))))

(deftest test-complex-json-with-partial-input
  (let [input0 (slurp "test/fixtures/complex.json")]
    (loop [consumer (parse json (take 100 input0))
           input (drop 100 input0)]
      (if (or (not (seq input))
              (done? consumer))
          (is (done? consumer))
          (recur (consumer (take 100 input))
                 (drop 100 input))))))

(deftest test-very-complex-json
  (let [input (slurp "test/fixtures/very-complex.json")
        result (parse-once json input)]
    (is (done? result))))

(deftest test-very-complex-json-with-partial-input
  (let [input0 (slurp "test/fixtures/very-complex.json")]
    (loop [consumer (parse json (take 1024 input0))
           input (drop 1024 input0)]
      (if (or (not (seq input))
              (done? consumer))
          (do
            (is (done? consumer)))
          (recur (consumer (take 1024 input))
                 (drop 1024 input))))))

(deftest test-js-string
  (let [result (parse-once js-string "\"hello world\"")]
    (is (done? result))
    (is (= "hello world" (:result result)))))

(deftest test-js-string-with-partial-input
  (let [partial-result (parse js-string "\"hello ")
        result (partial-result "world\"")]
    (is (done? result)
    (is (= "hello world" (:result result))))))

(deftest test-js-array
  (let [result (parse-once js-array "[\"hello\",
                                      [32, 42],
                                      43,
                                      \"other\"]")]
    (is (done? result))
    (is (= ["hello" [32 42] 43 "other"] (:result result)))))

(deftest test-js-array-with-partial-input
  (let [partial-result (parse js-array "[\"hello\",
                                        [32, 42],
                                        4")
        result (partial-result "3, \"other\"]")]
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

(deftest test-js-object
  (let [partial-result (parse js-object "{ \"name\": \"John Doe\",
                                   \"professional\": false,
                                   \"background\": null,
                                   \"age\": 43,
                                   \"phones\": [\"555-5555\",
                                                \"666-6")

         result (partial-result "666\"],
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

