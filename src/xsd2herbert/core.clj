(ns xsd2herbert.core
  (:require [miner.herbert.predicates :refer [str?]]
            [clojure.data.xml :refer [parse-str]])
  (:import (clojure.data.xml Element)))

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
  "Herbert predicat that returns true if s is in xs"
  [xs s] (xs s))

(defmulti basetype->herbert
          "convert a base type to an Herbert predicate"
          (fn [base] base))

(defmethod basetype->herbert "xsd:string" [_]
  '(pred str?))

(defn enumerations->herbert
  "Convert a list of enumerations to an Herbert predicate"
  [enumerations]
  {:pre [(not-any? #(not= :enumeration (:tag %)) enumerations)]}
  (let [values (->> (map #(get-in % [:attrs :value]) enumerations)
                    (reduce conj #{}))]
    (list 'pred 'in? values)))

(defn handle-restriction
  "Convert an XSD restriction of a type to a list of Herbert predicates"
  [restriction]
  {:pre (= :restriction (:tag restriction))}
  (let [base (-> (get-in restriction [:attrs :base])
                 basetype->herbert)
        enumerations (->> (:content restriction)
                          (filter (partial #(= :enumeration (:tag %))))
                          enumerations->herbert)]
    [base enumerations]))



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

(defn xms->herbert
  "Take a string and returns a seq of Herbert type definition"
  [^String xml]
  (->> (parse-str xml)
       (find-schemas)
       (map :content)
       (apply concat)
       (map define-simple-type)))
