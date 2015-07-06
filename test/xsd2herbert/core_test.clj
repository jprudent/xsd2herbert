(ns xsd2herbert.core-test
  (:require [clojure.test :refer :all]
            [xsd2herbert.core :refer :all]
            [miner.herbert.predicates :refer [str?]]
            [miner.herbert :refer [conform conforms?]]
            [clojure.data.xml :refer [parse-str]]))

(def simpleType-restriction-string-enumeration "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
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


(deftest should-find-schemas
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


(deftest should-define-simple-type
  (testing "a single simple type which is a restriction of string by enumeration"
    (let [simple-type (define-type(-> (parse-str simpleType-restriction-string-enumeration)
                                              (get-in [:content])
                                              first))]
      (is (= simple-type {:type    "DoseUnit"
                          :herbert '(and (pred str?) (pred in? #{"MG" "G" "ML" "MCG" "U" "KU", "MU" "MMOL"}))})))))

(deftest should-handle-regex
  (testing "antislash are doubled
  (but hey we can't write backslash-d in a string so they are automatically handled !)
   and ^ and $ are implicit in XSD"
    (is (= "^\\d$" (xsd-pattern->herbert "\\d"))))
  (testing "branches are grouped and have ^ and $ properly set"
    ;; fixme : finally, I am not so sure about this handling of ^ and $ ... ^ and $ aren't also implicit when re-matches ?
    (is (= "^(A|B|\\|)$|^\\|$|^C$|^other$" (xsd-pattern->herbert "(A|B|\\|)|\\|C|other")))))

(deftest should-handle-whitespace
  (testing "preserve"
    (is (whiteSpace? :preserve "   \u0009  he\u000Al\u000Do")))
  (testing "preserve"
    (is (not (whiteSpace? :replace "\u0009")))
    (is (not (whiteSpace? :replace "\u000A")))
    (is (not (whiteSpace? :replace "\u000D")))
    (is (whiteSpace? :replace "      "))
    (is (whiteSpace? :replace "a b c")))
  (testing "preserve"
    (is (not (whiteSpace? :collapse "\u0009")))
    (is (not (whiteSpace? :collapse "\u000A")))
    (is (not (whiteSpace? :collapse "\u000D")))
    (is (not (whiteSpace? :collapse "      ")))
    (is (not (whiteSpace? :collapse "   aaa   ")))
    (is (not (whiteSpace? :collapse "a      b")))
    (is (whiteSpace? :collapse "a b c"))))

(deftest should-handle-duration
  (testing "full negative duration"
    (is (re-matches duration-regex "-P99.5Y99.5M99.5DT12.2H12.2M22.2S")))
  (testing "full positive duration"
    (is (re-matches duration-regex "P99.5Y99.5M99.5DT12.2H12.2M22.2S")))
  (testing "without millis"
    (is (re-matches duration-regex "-P99Y99M99DT12H12M22S")))
  (testing "only years"
    (is (re-matches duration-regex "-P99.5Y")))
  (testing "only month"
    (is (re-matches duration-regex "-P99.5M")))
  (testing "only days"
    (is (re-matches duration-regex "-P99.5D")))
  (testing "only hours"
    (is (re-matches duration-regex "-PT12.2H")))
  (testing "only minutes"
    (is (re-matches duration-regex "-PT12.2M")))
  (testing "only seconds"
    (is (re-matches duration-regex "-PT22.2S"))))

(def simpleType-restriction-string-length "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='productCode'>
       <restriction base='string'>
         <length value='8' fixed='true'/>
         <length value='8' fixed='true'/>
       </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-pattern "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='SKU'>
        <restriction base='string'>
            <pattern value='\\d{3}-[A-Z]{2}'/>
            <pattern value='P\\p{Nd}{4}Y\\p{Nd}{2}M'/>
        </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-minLength "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='non-empty-string'>
      <restriction base='string'>
        <minLength value='1'/>
        <minLength value='1'/>
      </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-maxLength "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='form-input'>
      <restriction base='string'>
        <maxLength value='50'/>
        <maxLength value='50'/>
      </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-whitespace-preserve "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='useless-type'>
        <restriction base='string'>
          <whiteSpace value='preserve'/>
          <whiteSpace value='preserve'/>
        </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-whitespace-replace "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='one-liner'>
        <restriction base='string'>
          <whiteSpace value='replace'/>
          <whiteSpace value='replace'/>
        </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-string-whitespace-collapse "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='html-like'>
        <restriction base='string'>
          <whiteSpace value='collapse'/>
          <whiteSpace value='collapse'/>
        </restriction>
    </simpleType>
</xsd:schema>")

(def simpleType-restriction-duration-pattern "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <simpleType name='SQL-Year-Month-Interval'>
        <restriction base='duration'>
          <pattern value='P\\p{Nd}{4}Y\\p{Nd}{2}M'/>
        </restriction>
    </simpleType>
</xsd:schema>")

(def simplified-complexType-complexContent-restriction-anyType-sequence "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
            attributeFormDefault=\"qualified\"
            elementFormDefault=\"qualified\"
            targetNamespace=\"urn:Vidal\">
    <xsd:complexType name=\"ArrayOfInt\">
        <xsd:sequence>
            <xsd:element maxOccurs=\"unbounded\" minOccurs=\"0\" name=\"int\" nillable=\"true\" type=\"xsd:int\"/>
        </xsd:sequence>
    </xsd:complexType>
</xsd:schema>")

(deftest should-convert-xsd-to-herbert
  (testing "simple type/restriction/string/length"
    (is (= (xml->herbert simpleType-restriction-string-length)
           [{:type    "productCode"
             :herbert '(and (pred str?) (and (cnt 8) (cnt 8)))}])))
  (testing "simple type/restriction/string/minLength"
    (is (= (xml->herbert simpleType-restriction-string-minLength)
           [{:type    "non-empty-string"
             :herbert '(and (pred str?) (and (pred minLength? 1) (pred minLength? 1)))}])))
  (testing "simple type/restriction/string/maxLength"
    (is (= (xml->herbert simpleType-restriction-string-maxLength)
           [{:type    "form-input"
             :herbert '(and (pred str?) (and (pred maxLength? 50) (pred maxLength? 50)))}])))
  (testing "simple type/restriction/string/enum"
    (is (= (xml->herbert simpleType-restriction-string-enumeration)
           [{:type    "DoseUnit"
             :herbert '(and (pred str?) (pred in? #{"MG" "G" "ML" "MCG" "U" "KU", "MU" "MMOL"}))}])))
  (testing "simple type/restriction/string/pattern
  (if there is several patterns, they are ORed (see 4.3.4.3)"
    (is (= (xml->herbert simpleType-restriction-string-pattern)
           [{:type    "SKU"
             :herbert '(and (pred str?) (or "^\\d{3}-[A-Z]{2}$" "^P\\p{Nd}{4}Y\\p{Nd}{2}M$"))}])))
  (testing "simple type/restriction/string/whitespace/preserve"
    (is (= (xml->herbert simpleType-restriction-string-whitespace-preserve)
           [{:type    "useless-type"
             :herbert '(and (pred str?) (and (pred whiteSpace? :preserve) (pred whiteSpace? :preserve)))}])))
  (testing "simple type/restriction/string/whitespace/replace"
    (is (= (xml->herbert simpleType-restriction-string-whitespace-replace)
           [{:type    "one-liner"
             :herbert '(and (pred str?) (and (pred whiteSpace? :replace) (pred whiteSpace? :replace)))}])))
  (testing "simple type/restriction/string/whitespace/collapse"
    (is (= (xml->herbert simpleType-restriction-string-whitespace-collapse)
           [{:type    "html-like"
             :herbert '(and (pred str?) (and (pred whiteSpace? :collapse) (pred whiteSpace? :collapse)))}])))
  (testing "simplified version of complexType/complexContent/restriction/anyType/sequence/element
  the simplified version only got complexType/sequence/int complexContent/restriction/anyType is implicit"
    (is (= (xml->herbert simplified-complexType-complexContent-restriction-anyType-sequence)
           [{:type    "ArrayOfInt"
             :herbert '(and (& (:= ELEMENTS [int*]) (when (<= 0 (count ELEMENTS) 2147483647))))}]))))
