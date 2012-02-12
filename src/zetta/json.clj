(ns zetta.json
  ^{
    :author "Roman Gonzalez"
    :doc "A JSON parser combinator."
  }

  (:refer-clojure :exclude [get char])
  (:require [clojure.string :as str])

  (:use [zetta.core
         :only (always fail-parser do-parser with-parser <$> <* *> <|>)]
        [zetta.parser.seq
         :only (satisfy? char whitespace string number
                get put want-input?)]
        [zetta.combinators
         :only (sep-by skip-many)]))

(defrecord ContinueScan [buffer])
(defrecord ScanFinished [item-count remainder])

(defn- continue-scan? [scanner]
  (instance? ContinueScan scanner))

(defn- scan-finished? [scanner]
  (instance? ScanFinished scanner))

(defn scan [state0 p]
  (letfn [
    (scanner [state0 item-count items0]
      (let [item  (first items0)
            items (rest items0)]
      (if (nil? item)
        ; ^ when there is no elements in the buffer, we need
        ; to ask for more (this is done in the go function)
        (ContinueScan. state0)
        (let [state1 (p state0 item)]
          (if (nil? state1)
              ; ^ when p returns nil, we stop the scan
            (ScanFinished. item-count items)
            (recur state1 (+ 1 item-count) items))))))

    (process-scanner [scan-acc state0]
      (do-parser
        [input get
         :let [scan-result (scanner state0 0 input)]
         :cond [

           (continue-scan? scan-result) [
             _ (put [])
             more (want-input?)

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
                      (concat (take (:item-count scan-result) input)
                                    scan-acc))]]]

        result))
  ]
  (do-parser
    [scans (process-scanner [] state0)
     :cond [
       (= (count scans) 1) [ result (always (first scans)) ]
       :else [ result (always (concat scans)) ]]]
    result)))

(def skip-spaces (skip-many whitespace))

(declare js-object_ js-array_ js-string_)

(def js-value
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
    (with-parser
      (<|> most number))))


(def ^:private js-string_
  (letfn [
    (scan-step [prev-was-backslash? current-char]
      (if prev-was-backslash?
        false
        (if (= current-char \")
          nil
          (= current-char \\))))
  ]
  (with-parser
    (<$> str/join (scan false scan-step)))))

(def js-string
  (with-parser
    (*> (char \") js-string_)))


(defn- js-array-values [val-parser]
  (with-parser
    (*> skip-spaces
        (<* (sep-by (<* val-parser skip-spaces)
                    (*> (char \,) skip-spaces))
            (char \])))))

(def ^:private js-array_
  (with-parser
    (js-array-values js-value)))

(def js-array
  (with-parser
    (*> (char \[) js-array_)))

(defn- js-object-values [key-parser val-parser]
  (let [parse-pair (with-parser
                     (<$> vector (<* key-parser skip-spaces)
                                 (*> (char \:)
                                     skip-spaces
                                     val-parser)))]
    (with-parser
      (<$> (comp #(into {} %)
                 #(map (fn [[k v]] [(keyword k) v]) %))
                 (*> skip-spaces
                     (<* (sep-by (<* parse-pair skip-spaces)
                                 (*> (char \,) skip-spaces))
                         (char \})))))))

(def ^:private js-object_
  (with-parser
    (js-object-values js-string js-value)))

(def js-object
  (with-parser
    (*> (char \{) js-object_)))

(def json
  (do-parser [
    ch (*> skip-spaces (char #{\{ \[}))
    :if (= ch \{)
    :then [ result js-object_ ]
    :else [ result js-array_  ]]
  result))


