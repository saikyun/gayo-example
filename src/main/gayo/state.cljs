(ns gayo.state
  (:require [reagent.core :as r]
            [clojure.string :as str])
  (:require-macros [gayo.state]))

(defonce watched-o nil)

(defn debug
  [o k v form-meta]
  (when (and (#{:cant-aim} k)
             (not= (get @(.-state o) k) v))
    (println "setting" k v form-meta)))

(defn clojurize
  [s]
  (-> (str/replace (name s) #".[A-Z]" (fn [[a v]] (str a "-" v)))
      str/lower-case
      keyword)
  )

(comment
  (clojurize "aB")
  (clojurize "JaBaDa")
  
  (str/replace "JaBa" #".[A-Z]" (fn [[a v]]
                                  (println a v)
                                  (str a "-" v)))
  )

(defn watch
  [o]
  (when-not (identical? o watched-o)
    (let [new-state (js->clj (.. o -state))
          new-state (into {} (map 
                              (fn [[k v]] [(clojurize k) v])
                              new-state))]
      (set! (.. o -state) (r/atom new-state)))
    (set! watched-o o)))

#_(defn ensure-state!
    [o]
    (when-not (.-state o)
      (set! (.-state o) #js {})))

(defn ensure-state!
  [o]
  (when-not (.-state o)
    (set! (.-state o) #js {})))
