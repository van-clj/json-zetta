# json-zetta

Provides a JSON parser implemented using the zetta-parser library.

The only benefit that this parser provides in comparision to clojure.data.json
is the possibility of starting to parse a string without having all the
input available (thanks to zetta-parser).


## Usage

```clojure
(ns example
  (:use [zetta.core :only (parse-once)]
        [zetta.json :only (json)]))

(parse-once json "{ \"name\": \"John Doe\",
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
                      \"country\": \"USA\"}}")

; Returns
; { :name "John Doe"
;    :age 43
;    :professional false
;    :background nil
;    :phones ["555-5555" "666-6666"]
;    :address {
;      :suite "3241"
;      :street "90210 \"Beverly\" Hills"
;      :city "Los Angeles"
;      :state "California"
;      :country "USA" }}
```

## License

Copyright (C) 2012 Roman Gonzalez.

Distributed under the Eclipse Public License, the same as Clojure.
