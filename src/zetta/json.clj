(ns zetta.json
  ^{
    :author "Roman Gonzalez"
    :doc "A JSON parser combinator on zetta-parser."
  }

  (:refer-clojure :exclude [get char])
  (:require [clojure.string :as str])

  (:use [zetta.core
         :only (always fail-parser do-parser <$> <* *> <|>)]
        [zetta.parser.seq
         :only (satisfy? char string number
                get put want-input?
                skip-whitespaces)]
        [zetta.combinators
         :only (sep-by skip-many)]))

(defrecord ContinueScan [buffer])
(defrecord ScanFinished [item-count remainder])

(defn- continue-scan?
  "Test when the scanning process will continue due to missing input."
  [scanner]
  (instance? ContinueScan scanner))

(defn- scan-finished?
  "Test when the scanning process has finished, either because the
   continuation function returned nil or a result."
  [scanner]
  (instance? ScanFinished scanner))

(defn scan
  "Read from the parser input until next-scan-step returns nil.
   next-scan-step receives the state0 or the state returned
   on the previous call, with the current item being parsed."
  [state0 next-scan-step]
  (letfn [
    (scanner [state0 item-count items0]
      (let [item  (first items0)
            items (rest items0)]
      (if (nil? item)
        ; ^ when there is no elements in the buffer, we need
        ; to ask for more (this is done in process-scanner)
        (ContinueScan. state0)
        (let [state1 (next-scan-step state0 item)]
          (if (nil? state1)
              ; ^ on nil, we stop the scan
            (ScanFinished. item-count items)
            (recur state1 (+ 1 item-count) items))))))

    (process-scanner [scan-acc state0]
      (do-parser
        [input get
         :let [scan-result (scanner state0 0 input)]
         :cond [

           (continue-scan? scan-result) [
             _ (put [])
             more want-input?

             :if more
             :then [
               result (process-scanner (concat scan-acc input)
                                       (:buffer scan-result))
             ]
             :else [
               result (always (concat scan-acc input))
             ]]

           (scan-finished? scan-result) [
             _ (put (:remainder scan-result))
             result (always
                      (concat scan-acc (take (:item-count scan-result) input)))]]]

        result))
  ]
  (do-parser
    [scans (process-scanner [] state0)
     :cond [
       (= (count scans) 1) [ result (always (first scans)) ]
       :else [ result (always (concat scans)) ]]]
    result)))

(declare js-object_ js-array_ js-string_)

(def js-value
  "Parse a javascript value, this could either be an object, an array,
   a string, a boolean, a number or a null value. You should use json parser
   in favor of this function when parsing, as this one relaxes the
   object-or-array requirement of RFC 4627."
  (let [
    most (do-parser [
           ch (char #{\{ \[ \" \f \t \n})
           :cond [
             (= ch \{) [ result js-object_ ]
             (= ch \[) [ result js-array_  ]
             (= ch \") [ result js-string_ ]
             (= ch \f) [ result (*> (string "alse") (always false)) ]
             (= ch \t) [ result (*> (string "rue") (always true)) ]
             (= ch \n) [ result (*> (string "ull") (always nil))  ]
             :else [ result (fail-parser "the imposible happened!") ]]
         ]
         result)]
    (<|> most number)))


(def ^:private js-string_
  (letfn [
    (scan-step [prev-was-backslash? current-char]
      (if prev-was-backslash?
        false
        (if (= current-char \")
          nil
          (= current-char \\))))
  ]
  (<$> str/join (scan false scan-step))))

(def js-string
  "Parse a JSON string."
  (*> (char \") js-string_))


(defn- js-array-values [val-parser]
  (*> skip-whitespaces
      (<* (sep-by (<* val-parser skip-whitespaces)
                  (*> (char \,) skip-whitespaces))
          (char \]))))

(def ^:private js-array_
  (js-array-values js-value))

(def js-array
  "Parse a JSON array."
  (*> (char \[) js-array_))

(defn- js-object-values [key-parser val-parser]
  (let [parse-pair (<$> vector (<* key-parser skip-whitespaces)
                               (*> (char \:)
                                   skip-whitespaces
                                   val-parser))]
    (<$> (comp #(into {} %)
               #(map (fn [[k v]] [(keyword k) v]) %))
               (*> skip-whitespaces
                   (<* (sep-by (<* parse-pair skip-whitespaces)
                               (*> (char \,) skip-whitespaces))
                       (char \}))))))

(def ^:private js-object_
  (js-object-values js-string js-value))

(def js-object
  "Parse a JSON object."
  (*> (char \{) js-object_))

(def json
  "Parse a top-level JSON value.  This must be either an object or
   an array, per RFC 4627."
  (do-parser [
    ch (*> skip-whitespaces (char #{\{ \[}))
    :if (= ch \{)
    :then [ result js-object_ ]
    :else [ result js-array_  ]]
  result))


