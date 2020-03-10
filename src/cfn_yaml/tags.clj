(ns cfn-yaml.tags
  (:require [clojure.string :as str]
            [clj-yaml.core :as yaml])
  (:import (org.yaml.snakeyaml.nodes ScalarNode SequenceNode MappingNode NodeTuple Tag NodeId  Node)
           (org.yaml.snakeyaml DumperOptions$ScalarStyle DumperOptions$FlowStyle)
           (org.yaml.snakeyaml.constructor Construct Constructor)
           (org.yaml.snakeyaml.representer Represent)))

(defprotocol IEdn
  (to-edn [_]))

(extend-type Object
  IEdn
  (to-edn [x] x))

(extend-type nil
  IEdn
  (to-edn [x] nil))

(defrecord !Sub [string bindings]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::Sub" (cond-> [(to-edn string)]
                 bindings
                 (conj (into {} (map (fn [[k v]] [(to-edn k) (to-edn v)])) bindings)))}))

(defrecord !Ref [logicalName]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Ref" logicalName}))

(defrecord !Cidr [ipBlock count cidrBits]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::Cidr" (mapv to-edn [ipBlock count cidrBits])}))

(defrecord !Base64 [valueToEncode]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::Base64" (to-edn valueToEncode)}))

;; https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-findinmap.html
(defrecord !FindInMap [mapName topLevelKey secondLevelKey]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::FindInMap" (mapv to-edn [mapName topLevelKey secondLevelKey])}))

;; https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-getatt.html
(defrecord !GetAtt [logicalNameOfResource attributeName]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::GetAtt" (mapv to-edn [logicalNameOfResource attributeName])}))

;; https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-join.html
(defrecord !Join [delimiter list-of-values]
  yaml/YAMLCodec
  (decode [data keywords]
    data)
  (encode [data]
    data)
  IEdn
  (to-edn [_]
    {"Fn::Join" [(to-edn delimiter) (mapv to-edn list-of-values)]}))

(defn constructors [get-constructor]
  (let [construct #(.construct ^Construct (get-constructor %) %)]
    (->> [[!Ref #(->!Ref (.getValue ^ScalarNode %))]
          [!Sub (fn [node]
                  (if (= NodeId/scalar (.getNodeId ^Node node))
                    (->!Sub (.getValue ^ScalarNode node) {})
                    (let [[tpl m] (.getValue ^SequenceNode node)]
                      (->!Sub (.getValue ^ScalarNode tpl)
                              (into {}
                                    (map (fn [^NodeTuple n]
                                           [(construct (.getValueNode n))]))
                                    (.getValue ^MappingNode m))))))]
          [!Cidr (fn [^SequenceNode node] (apply ->!Cidr (map construct (.getValue node))))]
          [!FindInMap (fn [^SequenceNode node] (apply ->!FindInMap (map construct (.getValue node))))]
          [!GetAtt (fn [^ScalarNode node] (apply ->!GetAtt (clojure.string/split (.getValue node) #"\.")))]
          [!Join (fn [^SequenceNode node] (let [[delimiter list-of-values] (map construct (.getValue node))]
                             (->!Join delimiter list-of-values)))]
          [!Base64 (fn [node]
                     (condp = (.getNodeId ^Node node)
                       NodeId/scalar (->!Base64 (.getValue ^ScalarNode node))
                       NodeId/mapping (->!Base64 (into {}
                                                       (map (fn [^NodeTuple node-tuple]
                                                              [(construct (.getKeyNode node-tuple))
                                                               (construct (.getValueNode node-tuple))]))
                                                       (.getValue ^MappingNode node)))))]]
         (into {} (map (fn [[klass f]]
                         [(Tag. (.getSimpleName ^Class klass)) (reify org.yaml.snakeyaml.constructor.Construct
                                                          (construct [this node]
                                                            (f node)))]))))))

(defn scalar-node [tag ^String value & {:keys [^org.yaml.snakeyaml.DumperOptions$ScalarStyle style] :or {style DumperOptions$ScalarStyle/PLAIN}}]
  (ScalarNode. (if (string? tag)
                 (Tag. ^String tag)
                 ^Tag tag)
               value
               nil
               nil
               style))


(defn representers [represent-data]
  (let [represent-map (fn [m & {:keys [tag] :or {tag Tag/MAP}}]
                        (MappingNode. ^Tag tag
                                      (java.util.Arrays/asList
                                       (into-array NodeTuple
                                                   (for [[k v] m] (NodeTuple. (represent-data k) (represent-data v)))))
                                     DumperOptions$FlowStyle/BLOCK))]
    (->> [[!Ref #(scalar-node "!Ref" (:logicalName %))]
          [!Sub (fn [{:keys [string bindings]}]
                  (if (empty? bindings)
                    (scalar-node "!Sub" string :style (if (.contains ^String string "\n")
                                                               DumperOptions$ScalarStyle/LITERAL
                                                               DumperOptions$ScalarStyle/PLAIN))
                    (SequenceNode. (Tag. "!Sub")
                                   (java.util.Arrays/asList (into-array ScalarNode [(scalar-node Tag/STR string) (represent-map bindings)] ))
                                   DumperOptions$FlowStyle/BLOCK)))]
          [!Cidr #(SequenceNode. (Tag. "!Cidr")
                                 (java.util.Arrays/asList
                                  (into-array ScalarNode
                                              [(scalar-node Tag/STR (:ipBlock %) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)
                                               (scalar-node Tag/INT (str (:count %)))
                                               (scalar-node Tag/INT (str (:cidrBits %)))]))
                                 DumperOptions$FlowStyle/FLOW)]
          [!FindInMap #(SequenceNode. (Tag. "!FindInMap")
                                      (java.util.Arrays/asList
                                       (into-array ScalarNode
                                                   [(scalar-node Tag/STR (:mapName %) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)
                                                    (scalar-node Tag/STR (:topLevelKey %) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)
                                                    (scalar-node Tag/STR (:secondLevelKey %) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)]))
                                      DumperOptions$FlowStyle/FLOW)]
          [!GetAtt #(scalar-node "!GetAtt" (str (:logicalNameOfResource %) "." (:attributeName %)))]
          #_[!Join #(SequenceNode. (Tag. "!Join")
                                 [(scalar-node Tag/STR (:delimiter %) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)
                                  (sequence-node Tag/SEQ (map  scalar-node-str (:list-of-values %)) :style DumperOptions$ScalarStyle/DOUBLE_QUOTED)]
                                 DumperOptions$FlowStyle/FLOW)]
          [!Base64 (fn [{:keys [valueToEncode]}]
                     (cond
                       (string? valueToEncode) (scalar-node "!Base64" valueToEncode)
                       (map? valueToEncode) (represent-map valueToEncode :tag (Tag. "!Base64"))))]]
         (into {} (map (fn [[klass f]]
                         [klass (reify org.yaml.snakeyaml.representer.Represent
                                  (representData [this data]
                                    (f data)))]))))))
