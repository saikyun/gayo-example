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
  (-> (str/replace (name s) #".[A-Z]" (fn [[a v]] (str a "-" (str/lower-case v))))
      keyword)
  )

(defn js-ize
  [s]
  (str/replace (name s) #"-[A-Za-z]" (fn [[a v]] (str/upper-case v))))

(comment
  (clojurize "aB")
  (clojurize "JaBaDa")
  
  (js-ize (clojurize "JaBaDa"))
  
  (str/replace "JaBa" #".[A-Z]" (fn [[a v]]
                                  (println a v)
                                  (str a "-" v)))
  )

(defn watch
  [o]
  (when-not (identical? o watched-o)
    (when watched-o
      (let [new-state (into {} (map 
                                (fn [[k v]] [(js-ize k) v])
                                @(.. watched-o -state)))
            new-state (clj->js new-state)]
        (set! (.. watched-o -state) new-state)))
    
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
