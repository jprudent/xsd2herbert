(ns xsd2herbert.core-test
  (:require [clojure.test :refer :all]
            [xsd2herbert.core :refer :all]
            [miner.herbert.predicates :refer [str?]]
            [clojure.data.xml :refer [parse-str]]))

(def single-simple-type-xsd "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <xsd:simpleType name=\"DoseUnit\">
        <xsd:restriction base=\"xsd:string\">
            <xsd:enumeration value=\"MG\"/>
            <xsd:enumeration value=\"G\"/>
            <xsd:enumeration value=\"ML\"/>
            <xsd:enumeration value=\"MCG\"/>
            <xsd:enumeration value=\"U\"/>
            <xsd:enumeration value=\"KU\"/>
            <xsd:enumeration value=\"MU\"/>
            <xsd:enumeration value=\"MMOL\"/>
        </xsd:restriction>
    </xsd:simpleType>
</xsd:schema>")


(deftest should-find-schema
  (testing "should find all schemas but not nested ones"
    (let [schemas (find-schemas {:tag     :foo
                                 :content [{:tag     :schema
                                            :content [{:tag :schema :name :nested}]
                                            :name    :top}
                                           {:tag     :bar
                                            :content [{:tag :tennis}
                                                      {:tag :schema :name :inner1}]}
                                           {:tag :schema :name :inner2}]})
          schema-names (set (map :name schemas))]
      (is (= #{:top :inner1 :inner2} schema-names)))))


(deftest should-define
  (testing "a single simple type which is a restriction of string by enumeration"
    (let [simple-type (define-simple-type (-> (parse-str single-simple-type-xsd)
                                              (get-in [:content])
                                              first))]
      (is (= simple-type {:type    "DoseUnit"
                          :herbert '(and (pred str?) (pred in? #{"MG" "G" "ML" "MCG" "U" "KU", "MU" "MMOL"}))})))))

(deftest should-convert-xsd-to-herbert
  (testing "simple type/restriction/string/enum"
    (let [xml single-simple-type-xsd]
      (is (= (xms->herbert xml)
             [{:type    "DoseUnit"
               :herbert '(and (pred str?) (pred in? #{"MG" "G" "ML" "MCG" "U" "KU", "MU" "MMOL"}))}])))))
