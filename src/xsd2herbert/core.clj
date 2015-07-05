(ns xsd2herbert.core
  (:require [miner.herbert.predicates :refer [str?]]
            [clojure.data.xml :refer [parse-str]])
  (:import (clojure.data.xml Element)))

(defn parse-int [^String s]
  (Integer/parseInt s))

(defn find-schemas
  "returns a set of all schemas found in xml document"
  [^Element parsed-xml]
  (let [schemas (atom #{})]
    (clojure.walk/prewalk
      (fn [x]
        (if (map? x)
          (if (= :schema (:tag x))
            (do (swap! schemas conj x)
                nil)
            (:content x))
          x))
      parsed-xml)
    @schemas))

(defn in?
  "Predicate that returns true if s is in xs"
  [xs s] (xs s))

;; could remove group capturing though :
(def ^:static duration-regex #"^(?<sign>\+|-)?P(?:(?:(?:(?<years>\d+(?:[,.]\d+)?)Y)?(?:(?<months>\d+(?:[.,]\d+)?)M)?(?:(?<days>\d+(?:[.,]\d+)?)D)?(?<time>T(?:(?<hours>\d+(?:[.,]\d+)?)H)?(?:(?<minutes>\d+(?:[.,]\d+)?)M)?(?:(?<seconds>\d+(?:[.,]\d+)?)S)?)?)|(?<weeks>\d+(?:[.,]\d+)?W))$")

(defn duration?
  "Predicate that returns true if x is an ISO 8601 duration"
  [^String x]
  (re-matches duration-regex x))

(defn minLength?
  "Predicates that returns true if s has lenth greater or equal n"
  [^long n ^String s]
  (>= n (count s)))

(defmulti whiteSpace?
          "PRedicate that returns true if s verifies the whiteSpace constraint"
          (fn [kind _] kind))
(defmethod whiteSpace? :preserve [_ _ ] true)
(defmethod whiteSpace? :replace [_ s]
   (not-any? #{\u0009 \u000A \u000D} s))
(defmethod whiteSpace? :collapse [_ s]
  (and (whiteSpace? :replace s)
       (not (re-matches #".*[\u0020]{2,}.*|^\u0020.*|^.*\u0020$" s))))

(defmulti basetype->herbert
          "convert a base type to an Herbert predicate"
          (fn [^String base] (last (clojure.string/split base #":"))))

(defn get-facets-values [facets]
  (map #(get-in % [:attrs :value]) facets))

(defmethod basetype->herbert "string" [_]
  '(pred str?))

(defmulti facets->herbert (fn [[facet-kind _]] facet-kind))

(defmethod facets->herbert :enumeration
  [[_ enumerations]]
  {:pre [(not-any? #(not= :enumeration (:tag %)) enumerations)]
   :doc "Convert a list of enumerations to an Herbert predicate"}
  (let [values (->> (get-facets-values enumerations)
                    (reduce conj #{}))]
    (list 'pred 'in? values)))                            ;; could be (or "foo" "bar")

(defmethod facets->herbert :length
  [[_ lengths]]
  {:pre [(not-any? #(not= :length (:tag %)) lengths)]
     :doc "Convert a list of lenth restriction to an Herbert predicate"}
  (->> (get-facets-values lengths)
       (map #(list 'cnt (parse-int %)))
       (reduce conj (list 'and))
       reverse))

(defmethod facets->herbert :minLength
  [[_ lengths]]
  {:pre [(not-any? #(not= :minLength (:tag %)) lengths)]
     :doc "Convert a list of minLength restrictions to an Herbert predicate"}
  (->> (get-facets-values lengths)
       (map #(list 'pred 'minLength? (parse-int %)))
       (reduce conj (list 'and))
       reverse))

(defmethod facets->herbert :maxLength
  [[_ lengths]]
  {:pre [(not-any? #(not= :maxLength (:tag %)) lengths)]
     :doc "Convert a list of maxLength restrictions to an Herbert predicate"}
  (->> (get-facets-values lengths)
       (map #(list 'pred 'maxLength? (parse-int %)))
       (reduce conj (list 'and))
       reverse))

(defn xsd-pattern->herbert [^String xsd-pattern]
  (str "^" xsd-pattern "$"))

(defmethod facets->herbert :pattern
  [[_ patterns]]
  {:pre [(not-any? #(not= :pattern (:tag %)) patterns)]
   :doc "Convert a list of patterns to an Herbert predicate"}
  (->> (get-facets-values patterns)
       (map xsd-pattern->herbert)
       (reduce conj (list 'or))
       reverse))

(defmethod facets->herbert :whiteSpace
  [[_ whitespaces]]
  {:pre [(not-any? #(not= :whiteSpace (:tag %)) whitespaces)]
   :doc "Convert a list of patterns to an Herbert predicate"}
  (->> (get-facets-values whitespaces)
       (map #(list 'pred 'whiteSpace? (keyword %)))
       (reduce conj (list 'and))
       reverse))

(defn handle-restriction
  "Convert an XSD restriction of a type to a list of Herbert predicates"
  [restriction]
  {:pre (= :restriction (:tag restriction))}
  (let [base (-> (get-in restriction [:attrs :base])
                 basetype->herbert)
        restrictions (->> (:content restriction)
                          (group-by :tag)
                          (map facets->herbert)
                          (apply concat))]
    [base restrictions]))

(defn define-simple-type
  "take an XSD simpleType and returns an Herbert type definition"
  [simple-type]
  {:pre  [(= (:tag simple-type) :simpleType)]
   :post [(= 'and (first (:herbert %)))]}
  (let [type (get-in simple-type [:attrs :name])
        herberts (->> (map handle-restriction (:content simple-type))
                      (apply concat)
                      (reduce conj (list 'and))
                      reverse)]
    {:type type, :herbert herberts}))

(defn xml->herbert
  "Take a string and returns a seq of Herbert type definition"
  [^String xml]
  (->> (parse-str xml)
       (find-schemas)
       (map :content)
       (apply concat)
       (map define-simple-type)))
