(ns anicapi.targets.expo
  (:require
   ["react-native" :as rn]
   
   [gayo.assets :refer [path]]
   
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
   
   [reagent.core :as r]     
   [anicapi.data2 :as data]
   [anicapi.data :as gd]
   [gayo.tweens :as tw]
   [anicapi.ui.focus :as f]
   [gayo.hooks :as h]
   ["expo-gl" :as expo-gl]
   [shadow.expo :as expo]     
   [gayo.internal :as internal]     
   
   [anicapi.game :as game]     
   
   [anicapi.specs]
   
   [miracle.save :refer-macros [save]]))

(defonce editor (js/require "../../../assets/hexagon-new-assets.gltf"))
(defonce gameboard (js/require "../../../assets/gameboard.gltf"))

;; must use defonce and must refresh full app so metro can fill these in
;; at live-reload time `require` does not exist and will cause errors
;; must use path relative to :output-dir

(comment
  (reset! data/decklists [])
  
  [:> error-boundary]
  
  [:> rn/StatusBar {:backgroundColor "dark-grey" :barStyle "dark-content"}])

(defonce route (r/atom nil))

(defn preload!
  []
  (-> (bmfont/init-text)
      (p/then (fn [[s texture]]
                #_(log! "shader loaded" s texture)  
                (set! t/shader s)
                (set! t/text-texture texture)))))

(comment
  (reset! route nil)
  
  
  (-> (.destroyContextAsync expo-gl/GLView gayo-data/glc)
      (.then #(log! "lul" %)))
  
  (internal/on-context-create gayo-data/glc :welcome)
  )

(defn campaign
  [k]
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
       (log! "create HOHO")
       (save :create-hoho)
       (set! gayo-data/dont-render true)
       (p/then (internal/on-context-create gl-context (path gameboard))
               (fn [scene]
                 (println "haa" scene)
                 (game/deal-with scene k))))}]])

(defn select-campaign
  [k]
  (when (and gayo-data/glc (some? @route))
    (if (= @route :deck-builder)
      (p/then (internal/on-context-create gayo-data/glc (path gameboard))
              #(game/deal-with % k))
      (gayo-data/rg! k)))
  (reset! route k)
  (set! gayo-data/update! #'game/update!)
  (set! gayo-data/move #'game/move)
  (set! gayo-data/start #'game/start)
  (set! gayo-data/release #'game/release))

(defn root []
  (r/with-let [_ (preload!)]
    [:> rn/SafeAreaView
     {:style {:flex 1, :padding-top (+ (.-statusBarHeight constants))}}
     (case @route
       :zero-to-city-builder
       (campaign @route)
       
       :borrow
       (campaign @route)
       
       :first-real-game
       (campaign @route)
       
       :world-map
       (campaign @route)
       
       :normal-game
       (campaign @route)
       
       :deck-builder
       (campaign @route)
       
       ;; default
       nil)]))

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
  (set! data/goto-world-map! #(select-campaign :world-map))
  (set! data/goto-deck-builder!
        (fn []
          
          ;; (h/clear-all-hooks!)
          ;; (f/clear-focus!)
          ;; (tw/clear-tweens!)
          ;; (t/reset-msgs!)
          ;; (remove-watch gd/db :db-watch)
          
          ;; (when gayo-data/scene
          ;;   (scene/clear-obj! gayo-data/scene))
          
          ;; (reset! gd/gos {})
          ;; (reset! gd/db {})
          
          (gayo-data/cleanup-scene!)
          
          (comment
            let [scene (case (-> c/configs conf-k :scene)
                         :editor editor
                         gameboard)])
          
          (if (and gayo-data/glc (some? @route))
            (when-not (= @route :deck-builder)
              (p/then (internal/on-context-create gayo-data/glc (path editor))
                      #(game/deal-with % :deck-builder))))
          (reset! route :deck-builder)
          (set! gayo-data/update! #'editor/update!)
          (set! gayo-data/move #'editor/move)
          (set! gayo-data/start #'editor/start)                    
          (set! gayo-data/release #'editor/release)))

  (start)
  
  (p/then (data/get-rep)
          (fn [{:keys [finished-levels]}]
            (if (or true (:payback finished-levels))
              (select-campaign :first-real-game)
              #_(data/goto-world-map!)
              (select-campaign :borrow #_ :zero-to-city-builder)))))
