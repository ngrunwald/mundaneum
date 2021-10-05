(ns mundaneum.entity
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [mundaneum.properties :as p]
            [mundaneum.query :as q]
            [tick.core :as t]))

(def base-url "https://www.wikidata.org/w/api.php")

(declare get-entities)

(defrecord WikidataEntity [label url]
  clojure.core.protocols.Navigable
  (nav [_ _ _] ((get-entities url) url)))

(defrecord CommonsMedia [id])

(defn make-wikidtata-entity [url]
  (map->WikidataEntity {:url url :label (or (q/label (name url))
                                            (name url))}))

(defn parse-wikidata-url [url]
  (let [[_ id] (re-find #"^http://www\.wikidata\.org/entity/(\w+)$" url)]
    (if id (make-wikidtata-entity id) url)))

(defn parse-date [d]
  (try
    (-> d
        (str/replace #"^\+" "")
        (t/instant))
    (catch Exception e
      d)))

(defmulti process-value :datatype)

(defmethod process-value "wikibase-item"
  [{:keys [datavalue]}]
  (if (= (:type datavalue) "wikibase-entityid")
    (make-wikidtata-entity (get-in datavalue [:value :id]))
    datavalue))

(defmethod process-value "string"
  [{:keys [datavalue]}]
  (datavalue :value))

(defmethod process-value "external-id"
  [{:keys [datavalue]}]
  (str (datavalue :value)))

(defmethod process-value "monolingualtext"
  [{:keys [datavalue]}]
  (datavalue :value))

(defmethod process-value "commonsMedia"
  [{:keys [datavalue]}]
  (->CommonsMedia (datavalue :value)))

(defmethod process-value "quantity"
  [{:keys [datavalue]}]
  (let [{:keys [amount unit]} (datavalue :value)]
    {:amount (read-string amount)
     :unit (parse-wikidata-url unit)}))

(defmethod process-value "time"
  [{:keys [datavalue]}]
  (some-> datavalue
          (:value)
          (:time)
          (parse-date)))

(defmethod process-value "url"
  [{:keys [datavalue]}]
  (datavalue :value))

(defmethod process-value :default
  [snak]
  snak)

(defn process-claim [c]
  (let [{:keys [mainsnak]} c]
    (process-value mainsnak)))

(defn process-entity [e]
  (let [{:keys [aliases descriptions modified claims]} e]
    (merge
     {:aliases aliases
      :descriptions descriptions
      :modified (t/instant modified)}
     (reduce (fn [acc [prop data]]
               (assoc acc
                      (or (p/urls (name prop))
                          (p/clean-keyword-text (q/label (name prop)))
                          (name prop))
                      (map process-claim data)))
             {} claims))))

(defn get-raw-entities [arg]
  (let [{:keys [ids languages]
         :or {languages [:en]}} (cond (map? arg) arg
                                      (seq? arg) {:ids arg}
                                      :else {:ids [arg]})]
    (-> (http/get base-url {:query-params {:action "wbgetentities"
                                           :ids (str/join "|" (map name ids))
                                           :languages (str/join "|" (map name languages))
                                           :format "json"}
                            :as :json})
        (:body)
        (:entities))))

(defn get-entities [arg]
  (let [{:keys [ids languages]
         :or {languages [:en]}} (cond (map? arg) arg
                                      (seq? arg) {:ids arg}
                                      :else {:ids [arg]})
        entities (get-raw-entities arg)]
    (reduce (fn [acc [id entity]]
              (assoc acc (name id) (process-entity entity)))
            {} entities)))
