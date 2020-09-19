(ns boat.targets.expo
  (:require
   ["react-native" :as rn]
   
   [gayo.path :refer [path]]
   
   [gayo.data :as gayo-data]
   
   [gayo.layout :as lay]            
   
   [reagent.core :as r]
   ["expo-gl" :as expo-gl]   
   [gayo.log :refer [log!]]
   ["expo-constants" :default constants]
   [shadow.expo :as expo]
   
   [gayo.bmfont :as bmfont]
   
   [promesa.core :as p]
   
   [gayo.bmfont :refer [default-opts-clj]]
   
   [gayo.text :as t]     
   
   [gayo.internal :as internal]
   
   [boat.game :as game]))

(defonce gameboard (js/require "../../../assets/scene.gltf"))

;; must use defonce and must refresh full app so metro can fill these in
;; at live-reload time `require` does not exist and will cause errors
;; must use path relative to :output-dir

(defn preload!
  []
  (-> (bmfont/init-text)
      (p/then (fn [[s texture]]
                #_(log! "shader loaded" s texture)  
                (set! t/shader s)
                (set! t/text-texture texture)))))

(defn root []
  (r/with-let [_ (preload!)]
    [:> rn/SafeAreaView
     {:style {:flex 1, :padding-top (+ (.-statusBarHeight constants))}}
     
     [:> rn/View
      (merge {:style {:flex 1}
              :on-layout #'lay/on-layout
              :onResponderGrant #(internal/grant %)
              
              :onStartShouldSetResponder (constantly true)}
             (if gayo-data/on-pc?
               {:onKeyDown #(internal/key-down %)
                :onWheel #(internal/mouse-wheel %)
                :onMouseMove #(internal/mouse-move %)
                :onMouseDown #(internal/mouse-down %)
                :onMouseUp #(internal/mouse-up %)}
               {:onTouchStart #(internal/touch-start %)
                :onResponderMove #(internal/touch-move %)
                :onResponderRelease #(internal/touch-release %)}))
      [:> expo-gl/GLView
       {:style {:flex 1}
        :on-context-create
        (fn [gl-context]
          
          (set! gayo-data/update! #'game/update!)
          (set! gayo-data/move #'game/move)        
          (set! gayo-data/start #'game/start)        
          (set! gayo-data/release #'game/release)  
          
          (log! "create HOHO")
          (set! gayo-data/dont-render true)
          (p/then (internal/on-context-create gl-context (path gameboard))
                  (fn [scene]
                    (println "haa" scene)
                    (internal/deal-with gl-context :first-real-game game/init))))}]]]))

(defn start
  "NOTE: after-load can't be on, because you can get
  strange errors regarding the gl-context.
  {:dev/after-load true}"
  []
  #_(expo/render-root
     (r/as-element
      [:> rn/SafeAreaView {:style {:flex 1}}
       [:> rn/Text {:style {:color "#00ff00"}} "lul"]
       [:> rn/Button {:title "hej hoho watter !!"}]]))
  (expo/render-root (r/as-element [root])))

(defn init []
  (start))
