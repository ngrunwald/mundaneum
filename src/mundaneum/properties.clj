(ns mundaneum.properties
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]))

;; Properties fetched using the wikibase command line tool:
;; https://github.com/maxlath/wikibase-cli
;; ... example invocation:
;; $ wb props -e https://query.wikidata.org/sparql > props-yyyy-mm-dd.json

(defn clean-keyword-text [text]
  (or (some-> text
              (str/replace #"[ /]" "-")
              (str/replace #"[\(\)\'\,]" "")
              keyword)
      text))

(let [[props ids] (->> (json/decode (slurp (io/resource "props-2021-09-28.json")))
                       (reduce (fn [acc [id text]]
                                 (let [k (clean-keyword-text text)]
                                   (-> acc
                                       (assoc-in [0 k] id)
                                       (assoc-in [1 id] k))))
                               [{} {}]))]
  (def properties props)
  (def urls ids))

