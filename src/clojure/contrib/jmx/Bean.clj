(ns clojure.contrib.jmx.Bean
  (:gen-class
   :implements [javax.management.DynamicMBean]
   :init init
   :state state
   :constructors {[Object] []})
  (:require [clojure.contrib.jmx :as jmx])
  (:import [javax.management DynamicMBean MBeanInfo AttributeList]))

(defn -init [derefable]
  [[] derefable])

; TODO: rest of the arguments, as needed
(defn generate-mbean-info [clj-bean]
  (MBeanInfo. (.. clj-bean getClass getName)                      ; class name
              "Clojure Dynamic MBean"                             ; description
              (jmx/map->attribute-infos @(.state clj-bean))       ; attributes
              nil                                                 ; constructors
              nil                                                 ; operations
              nil))                                               ; notifications                                          

(defn -getMBeanInfo
  [this]
  (generate-mbean-info this))

(defn -getAttribute
  [this attr]
  ((.state this) (keyword attr)))

(defn -getAttributes
  [this attrs]
  (let [result (AttributeList.)]
    (doseq [attr attrs]
      (.add result (.getAttribute this attr)))
    result))