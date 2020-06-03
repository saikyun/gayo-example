(ns gayo.storage
  (:require #?@(:rn  [["react-native" :as rn]])))


#?(:rn (do
         (defn store
           [k v]
           (.. rn/AsyncStorage (setItem k v)))
         
         (defn load
           [k]
           (.. rn/AsyncStorage (getItem k))))
   
   :browser (do
              (defn store
                [k v]
                (println "TODO: Implement store")
                (.resolve js/Promise nil))
              
              (defn load
                [k]
                (println "TODO: Implement load")
                (.resolve js/Promise nil))))



