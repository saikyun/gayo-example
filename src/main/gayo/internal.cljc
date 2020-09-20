(ns gayo.internal
  (:require ["three" :as THREE]
            [gayo.three-utils :as three-utils]
            ["threestuff/GLTFLoader" :as gltf]
            
            [gayo.printers]
            [gayo.data :as gayo-data]
            
            [clojure.string :as str]
            
            [gayo.log :refer [log!]]
            [gayo.path :as path]
            
            [gayo.assets :as a]
            [gayo.data :as gayo-data]
            [gayo.multiple-views :as mv]

            [gayo.tweens :as tw]
            [gayo.bmfont :as bmfont]
            [gayo.text :as t]

            [gayo.composer :as cmp]
            [gayo.scene :as scene]

            [gayo.hooks :as h]
            [gayo.particles :as fx]

            [gayo.animation :as anim]
            [promesa.core :as p]

            [clojure.pprint :refer [pp]]
            [cljs-bean.core :refer [bean ->clj ->js]]

            [miracle.save :refer-macros [save save-do]]))

(defn grant
  [_]
  ;; ??
  )

(def point (THREE/Vector2. 0 0))

(def down false)

(defn deal-with
  [loaded-gltf conf-k init-f]
  (set! gayo-data/loading true)
  
  (println "dealing...")
  
  (let [ld-scene (.. loaded-gltf -scene)]
    (doseq [c (into [] (.-children ld-scene))]
      (.add gayo-data/scene c)))
  
  (comment
    (.-children gayo-data/scene)
    )
  
  ;; Checks custom data, e.g. transparency  
  (.. gayo-data/scene (traverse scene/handle-attributes!))  
  
  (init-f gayo-data/scene loaded-gltf conf-k)
  
  (set! gayo-data/loading false)
  (set! gayo-data/dont-render false))

(defn mouse-move
  [ev]
  (when down
    (let [x (- #?(:browser  (.. ev -pageX)
                  :rn (.. ev -nativeEvent -pageX))
               (.-x gayo-data/dims))   
          y (- #?(:browser  (.. ev -pageY)
                  :rn (.. ev -nativeEvent -pageY))
               (.-y gayo-data/dims))
          w (.-z gayo-data/dims)
          h (.-w gayo-data/dims)]
      (set! (.-x point) (* 2 (- (/ x w) 0.5)))
      (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5))))
    (gayo-data/move point gayo-data/scene (.-camera gayo-data/view))))

(defn mouse-down
  [ev]
  (set! down true)
  
  (save :hutneso)
  (let [x (- #?(:browser  (.. ev -pageX)
                :rn (.. ev -nativeEvent -pageX))
             (.-x gayo-data/dims))  
        y (- #?(:browser  (.. ev -pageY)
                :rn (.. ev -nativeEvent -pageY))
             (.-y gayo-data/dims))
        w (.-z gayo-data/dims)
        h (.-w gayo-data/dims)]
    
    
    (set! (.-x point) (* 2 (- (/ x w) 0.5)))
    (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5)))
    
    (println point))
  
  (gayo-data/start point gayo-data/scene (.-camera gayo-data/view)))

(defn mouse-up
  [ev]
  (set! down false)
  (let [x (- #?(:browser  (.. ev -pageX)
                :rn (.. ev -nativeEvent -pageX))
             (.-x gayo-data/dims)) 
        y (- #?(:browser  (.. ev -pageY)
                :rn (.. ev -nativeEvent -pageY))
             (.-y gayo-data/dims))
        w (.-z gayo-data/dims)
        h (.-w gayo-data/dims)]
    (set! (.-x point) (* 2 (- (/ x w) 0.5)))
    (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5)))
    (gayo-data/release point gayo-data/scene (.-camera gayo-data/view))))

(defn key-down
  [ev]
  (println ev)
  ;;      (boat.game/try-shoot boat.game/main-char :k)
  )

;;(.addEventListener js/window "keydown" key-down)

(defn mouse-wheel
  [ev]
  #_  (data/add-event! ev))

(defn touch-ev->obj
  [ev type]
  (when-let [t (.. ev -nativeEvent)]
    (def aa t)
    (let [t (if (.-screenX t) t
                (or (some-> t .-touches (aget 0))
                    (some-> t .-changedTouches (aget 0))))
          coo #js {:pageX (.-pageX t)
                   :pageY (.-pageY t)
                   :type type}]
      coo)))

(defn touch-start
  [ev]
  (set! down true)
  (let [x (- (.. ev -nativeEvent -pageX) (.-x gayo-data/dims))
        y (- (.. ev -nativeEvent -pageY) (.-y gayo-data/dims))
        w (.-z gayo-data/dims)
        h (.-w gayo-data/dims)]
    (set! (.-x point) (* 2 (- (/ x w) 0.5)))
    (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5))))

  (gayo-data/start point gayo-data/scene (.-camera gayo-data/view)))

(defn touch-move
  [ev]
  (when down
    (let [x (- (.. ev -nativeEvent -pageX) (.-x gayo-data/dims))
          y (- (.. ev -nativeEvent -pageY) (.-y gayo-data/dims))
          w (.-z gayo-data/dims)
          h (.-w gayo-data/dims)]
      (set! (.-x point) (* 2 (- (/ x w) 0.5)))
      (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5))))

    (gayo-data/move point gayo-data/scene (.-camera gayo-data/view))))

(defn touch-release
  [ev]
  (set! down false)
  (let [x (- (.. ev -nativeEvent -pageX) (.-x gayo-data/dims))
        y (- (.. ev -nativeEvent -pageY) (.-y gayo-data/dims))
        w (.-z gayo-data/dims)
        h (.-w gayo-data/dims)]
    (set! (.-x point) (* 2 (- (/ x w) 0.5)))
    (set! (.-y point) (* 2 (- (- 1 (/ y h)) 0.5)))
    (gayo-data/release point gayo-data/scene (.-camera gayo-data/view))))

(defonce gltf-loader (gltf/GLTFLoader.))
(defonce gltf-loader2 (gltf/GLTFLoader.))

(set! (.-textureLoader gltf-loader2) a/texture-loader)

(comment
  (bean gltf-loader)
  (bean gltf-loader2)

  ls

  (scene/clear-obj! gayo-data/scene)

  (.load gltf-loader2
         (if rn/Image.resolveAssetSource
           (.-uri (rn/Image.resolveAssetSource gameboard))
           gameboard)
         (fn [scene]
           (log! "yee" scene)
           (def ls scene)
           #_  (swap! loaded-scenes assoc conf-k scene)
           )
         #(log! "progress" (.-loaded %) (.-total %))
         #(do (js/console.log %) (throw %)))
  
  )

(def fps-count 0)
(def fps-sum 0)

(defn render!
  [dt]
  (when-not gayo-data/dont-render
    (let [fps (js/Math.round (/ 1000.0 dt))]
      (set! fps-count (inc fps-count))
      (set! fps-sum (+ fps-sum fps))
      
      #_(when (<= 10 fps-count)
          (let [fps-s (str (/ fps-sum fps-count))]
            (bmfont/update-text* gayo-data/fps-text fps-s)
            (set! fps-count 0)
            (set! fps-sum 0)))
      
      (let [camera (.-camera gayo-data/view)]
        (if-not gayo-data/loading
          (do
            (gayo-data/update! gayo-data/scene camera dt)
            (anim/update-animations! 16 #_ dt)
            (tw/update-tweens! #_ 16 dt))
          (log! "loading!"))
        
        (h/run-hooks! :update {:dt dt})
        (h/run-hooks! :second-update {:dt dt})      
        
        (.render gayo-data/composer))
      
      #?(:rn (.endFrameEXP gayo-data/glc)))))

(defn next-frame
  [curr-time]
  (when-not gayo-data/last-time
    (set! gayo-data/last-time curr-time))
  
  (if (and
       #_false
       js/document
       (.-hasFocus js/document)
       (not (.hasFocus js/document)))
    (js/setTimeout #(js/requestAnimationFrame next-frame) 1000)
    (js/requestAnimationFrame next-frame))
  
  (render! (- curr-time gayo-data/last-time))
  (set! gayo-data/last-time curr-time))

(defonce next-frame-running false)
(defonce loaded-scenes (atom {}))

(defn on-scene-load
  [loaded-scene]
  (gayo-data/init-animations (.-animations loaded-scene))
  (set! gayo-data/loaded-scene loaded-scene)
  
  (when-not next-frame-running
    (set! next-frame-running true)
    (next-frame 0))
  
  (p/resolved loaded-scene))

(defn init-scene!
  [path]
  (set! (.-preloaded js/window) a/assets)
  (set! (.-textureLoader gltf-loader) a/texture-loader)
  
  (save :huuh-load?)
  
  (log! "gonna load with gltf :D")
  
  (let [p (p/deferred)]
    (if-let [s (get @loaded-scenes path)]
      (-> (on-scene-load s)
          (p/then #(p/resolve! p %)))
      (do (swap! loaded-scenes assoc path :loading)
          (.load gltf-loader
                 path
                 (fn [scene]
                   (swap! loaded-scenes assoc path scene)
                   (-> (on-scene-load scene)
                       (p/then #(p/resolve! p %))))
                 #(log! "progress" (.-loaded %) (.-total %))
                 #(do (js/console.log %) (throw %)))))
    p))

#?(:browser (defn resize
              []
              (let [iw (.. js/window -innerWidth)
                    ih (.. js/window -innerHeight)
                    cam (.-camera gayo-data/view)]
                
                
                (set! (.-x gayo-data/dims) 0)
                (set! (.-y gayo-data/dims) 0)      
                
                (set! (.-z gayo-data/dims) iw)
                (set! (.-w gayo-data/dims) ih)
                (set! (.-h gayo-data/dims) 1)
                
                (set! (.. cam -aspect)  (/ iw ih))
                (.. cam (updateProjectionMatrix))
                (.. gayo-data/renderer (setSize iw ih)))))

(defn on-context-create
  [gl path]
  (set! gayo-data/glc gl)
  
  (when-not gayo-data/scene
    (set! gayo-data/scene (THREE/Scene.)))
  
  (set! gayo-data/renderer (three-utils/renderer gl))  
  
  (when-not gl
    (set! gayo-data/glc (.getContext gayo-data/renderer)))  
  
  (set! gayo-data/max-ani (.. gayo-data/renderer -capabilities getMaxAnisotropy))
  
  (let [gl (or gl gayo-data/glc)]
    (set! gayo-data/loading true)
    
    (log! "Fixing scene...")
    
    (when gayo-data/scene
      (log! "clearing old")
      (scene/clear-obj! gayo-data/scene))
    
    (mv/init gl gayo-data/scene)
    
    #?(:browser (do
                  (.. js/window (addEventListener "resize" resize false))
                  (resize)
                  
                  (let [app (.-domElement gayo-data/renderer)]
                    (doseq [[k v] {:onKeyDown #(key-down %)
                                   :onWheel #(mouse-wheel %)
                                   :onMouseMove #(mouse-move %)
                                   :onMouseDown #(mouse-down %)
                                   :onMouseUp #(mouse-up %)}]
                      (aset app (str/lower-case (name k)) v)))))
    
    (set! gayo-data/composer
          (cmp/composer gayo-data/renderer
                        gayo-data/scene
                        (.-camera gayo-data/view)))
    
    (.setClearColor gayo-data/renderer "rgb(40, 40, 80)")
    
    (init-scene! path)))
