(ns gayo.layout
  (:require [gayo.log :refer [log!]]
            [gayo.data :as gayo-data]
            [miracle.save :refer-macros [save]]))

(goog-define ^boolean RN true)



(if RN
  (do
    (def rn (js/require "react-native"))
    
    (def to nil)
    
    (defn refresh-layout!
      []
      (when gayo-data/glc
        (let [real-w (.. rn -PixelRatio (getPixelSizeForLayoutSize (.-z gayo-data/dims)))
              real-h (.. rn  -PixelRatio (getPixelSizeForLayoutSize (.-w gayo-data/dims)))]
          (log! "w" real-w ", h" real-h)
          
          (let [c gayo-data/glc #_ (.getContext gayo-data/renderer)]
            (when-not c 
              (save :wtf-no-context)
              )
            
            (log! "bw" (.-drawingBufferWidth c))
            (log! "bh" (.-drawingBufferHeight c))
            (log! "w" (.. c -canvas -width))
            (log! "h" (.. c -canvas -height))
            
            (some-> to js/clearTimeout)
            
            (let [ratio (/ real-w real-h)
                  min-ratio 0.57
                  ratio (if (< ratio min-ratio)
                          min-ratio
                          ratio)
                  real-h (/ real-w ratio)]
              
              (set! (.-w gayo-data/dims) 
                    (/ real-h 
                       (.-h gayo-data/dims)))
              
              (set! to
                    (js/setTimeout
                     (fn []
                       
                       (if-not c 
                         (do
                           (save :wtf-no-context2)
                           (println "NO CONTEXT"))
                         
                         (do
                           
                           
                           (let [scale (.-h gayo-data/dims)]
                             (set! (.. c -canvas -width) real-w)
                             (set! (.. c -canvas -height) real-h)
                             
                             (set! (.. c -canvas -style -width) (str (/ real-w scale) "px"))
                             (set! (.. c -canvas -style -height) (str (/ real-h scale) "px")))
                           
                           (when-let [cam (some-> gayo-data/view .-camera)]
                             (set! (.-aspect cam) ratio)
                             (.updateProjectionMatrix cam))        
                           
                           (when-let [r gayo-data/renderer]
                             (.setSize r real-w real-h)
                             (.setPixelRatio r 1)
                             #_(.setSize r (* 0.5 real-w) (* 0.5 real-h))))))
                     300)))))))
    
    (defn on-layout
      [e]
      (set! (.-x gayo-data/dims)  (.. e -nativeEvent -layout -x))
      (set! (.-y gayo-data/dims) (.. e -nativeEvent -layout -y))      
      
      (let [w (.. rn -Dimensions (get "window"))
            scale (.-scale w)]
        (set! (.-z gayo-data/dims) (.-width w))
        (set! (.-w gayo-data/dims) (.-height w))
        (set! (.-h gayo-data/dims) scale))
      
      (refresh-layout!)))
  

  (defn on-layout
    [e]
    (log! "hurp"))
  
  
  )

