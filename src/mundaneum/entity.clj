(ns mundaneum.entity
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [mundaneum.properties :as p]
            [mundaneum.query :as q]
            [tick.core :as t]
            [clojure.java.browse :as browse]))

(def base-url "https://www.wikidata.org/w/api.php")

(declare get-entities)

(defrecord WikidataEntity [label url]
  clojure.core.protocols.Navigable
  (nav [_ _ _] ((get-entities url) url)))

(defn commons-media-url-from-filename [filename]
  (let [res (http/get "https://en.wikipedia.org/w/api.php"
                      {:query-params {:action "query"
                                      :titles (str "File:" filename)
                                      :prop "imageinfo"
                                      :iiprop "url"
                                      :format "json"}
                       :as :json})]
    (get-in res [:body :query :pages :-1 :imageinfo 0 :url])))

(defn browse-media-commons [media]
  (browse/browse-url (commons-media-url-from-filename (:filename media))))

(defrecord CommonsMedia [filename])

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
  (when-let [filename (:value datavalue)]
    (->CommonsMedia filename)))

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
              (assoc acc (name id) (assoc (process-entity entity)
                                          :id (name id))))
            {} entities)))

(defn get-entity [arg]
  (-> (cond
        (instance? WikidataEntity arg) {:ids [(:url arg)]}
        :else {:ids [arg]})
      (get-entities)
      (vals)
      (first)))

(defn annotate-map-entity [k entity]
  (vary-meta entity
             merge
             {'clojure.core.protocols/nav (fn [ent _ _] (if-let [id (get ent k)]
                                                          (get-entity id)
                                                          ent))}))

(defn search-raw-entities [arg]
  (let [{:keys [query language limit continue]
         :or {language :en} :as arg-map} (cond (map? arg) arg
                                               (string? arg) {:query arg}
                                               :else {:ids [arg]})]
    (-> (http/get base-url {:query-params (merge
                                           {:action "wbsearchentities"
                                            :search query
                                            :language (name language)
                                            :format "json"}
                                           (select-keys arg-map [:limit :continue]))
                            :as :json})
        (:body))))

(defn search-entities [arg]
  (let [entities (:search (search-raw-entities arg))]
    (map (partial annotate-map-entity :id) entities)))

