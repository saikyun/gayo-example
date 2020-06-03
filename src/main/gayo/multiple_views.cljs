(ns gayo.multiple-views
  (:require ["three" :as THREE]
            [gayo.log :refer [log!]]
            [gayo.data :as gayo-data :refer [dims views]]
            [cljs-bean.core :refer [bean]]
            ["threestuff/OrbitControls" :as oc]))

(defn single?
  []
  true)

(def initial-views
  [{:left 0
    :name "Game View"
    :bottom 0
    :width (if single? 1 0.5)
    :height 1.0
    :background (THREE/Color. 0.5 0.5 0.7)
    :eye #js [0 17.5 5.3]
    :up #js [0 1 0]
    :input #js {:point #js {:x 0 :y 0}}
    :fov 30
    :lastEvent nil
    :mouseDown false
    
    :init
    (fn [view]
      (set! (.. view -camera -rotation -x) -1.3)
      (set! (.. view -camera -name) "Game Camera")
      #_      (.lookAt camera (THREE/Vector3. 0 0 0)))}
   
   {:left 0.5
    :bottom 0
    :controls nil
    :name "Scene View"
    :width 0.5
    :height 1.0
    :background (THREE/Color. 0.7 0.5 0.5)
    :eye #js [10 20 20]
    :up #js [0 0 -1]
    :fov 45
    :mouseDown false
    :lastEvent nil
    :init (fn [view]
            (log! "THIS SHOULD ONLY RUN ON PC")
            #_            (let [stats (Stats)]
                            (set! (.-stats view)
                                  stats)
                            
                            (set! (.. stats -domElement -style -position) "absolute")
                            (set! (.. stats -domElement -style -right) "0px")
                            (set! (.. stats -domElement -style -top) "0px")
                            (.. js/document -body (appendChild (.-domElement stats))))
            
            (set! (.-controls view)
                  (oc/OrbitControls. (.-camera view))))
    :handleInput    
    (fn [scene camera view new-event]
      (when-let [controls (.-controls view)]
        (when (not= new-event (.-lastEvent view))
          (case (.-type new-event)
            "mousedown" (do (set! (.-mouseDown view) true)
                            (.handleMouseDownRotate controls new-event))
            "mousemove" (when (.-mouseDown view)
                          (.handleMouseMoveRotate controls new-event))
            "mouseup" (set! (.-mouseDown view) false)
            "wheel" (.handleMouseWheel controls new-event)
            (log! "de nada")))
        
        (set! (.-lastEvent view) new-event)))
    :update
    (fn [renderer scene camera view]
      #_      (.. view -stats (update renderer));
      (.. view -controls update))}])

(comment
  
  
  (bean (.-controls (last views)))
  
  
  )

(defn initiate-views
  ([]
   (initiate-views #js []))
  ([views]
   (let [len (if (single?) 1 (count initial-views))]
     (set! (.-length views) len)
     (doseq [i (range len)
             :let [iv (nth initial-views i)
                   v (aget views i)]]
       (aset views i (clj->js iv))))
   (set! gayo-data/view (first views))
   views))

(defn init
  [gl scene]
  (set! views (initiate-views views))
  (doseq [view views
          :let [camera (THREE/PerspectiveCamera. (.-fov view)
                                                 (/ gl.drawingBufferWidth gl.drawingBufferHeight)
                                                 1
                                                 10000)]]
    (log! "view" (.-eye view))
    (.. camera -position (fromArray (.-eye view)))
    (.. camera -up (fromArray (.-up view)))
    (.add scene camera)
    (set! (.-camera view) camera)
    (when (.-init view)
      (.init view view))))

(defn is-view-locked?
  [view]
  (.-mouseDown view))

(defn render
  [scene renderer views input-queue huhu]
  (if false
    (let [view (first views)
          {:keys [camera update handleInput left bottom width height background]} (bean view)]
      #_      (doseq [ev input-queue
                      :let [x (.-pageX ev)
                            y (.-pageY ev)
                            x-percent (/ x (.-width dims))]]
                
                (handleInput scene camera view ev))
      
      (update renderer scene camera view)
      
      #_        (.setScissorTest renderer false)
      #_        (.setClearColor renderer background)          
      
      #_        (set! (.-aspect huhu) (/ w h))
      #_      (.updateProjectionMatrix camera)
      
      #_(let [l (js/Math.floor (.-width dims))
              b (js/Math.floor (.-height dims))
              w (js/Math.floor (.-width dims))
              h (js/Math.floor (.-height dims))]
          
          (.setViewport renderer l b w h)
          (.setScissor renderer l b w h)          
          (.setScissorTest renderer true)          
          
          (set! (.-aspect camera) (/ w h))          
          (.updateProjectionMatrix camera)          
          
          (.render renderer scene camera))
      
      (def c1 huhu)
      (def c2 camera)
      
      (.render renderer scene camera))
    
    (let [locked-view (first (filter #(is-view-locked? %) views))]
      (doseq [view views
              :let [{:keys [camera update handleInput left bottom width height background]} (bean view)]]
        (when (or (not locked-view)
                  (= locked-view view))
          
          (doseq [ev input-queue
                  :let [x (.-pageX ev)
                        y (.-pageY ev)
                        x-percent (/ x (.-width dims))]]
            
            (when (or locked-view
                      (and (> x-percent left)
                           (< x-percent (+ left width))))
              (handleInput scene camera view ev))
            
            (comment      (handleInput view input-queue))
            
            ))
        
        (update renderer scene camera view)
        
        (let [l (js/Math.floor (* (.-width dims) left))
              b (js/Math.floor (* (.-height dims) bottom))
              w (js/Math.floor (* (.-width dims) width))
              h (js/Math.floor (* (.-height dims) height))]
          
          (.setViewport renderer l b w h)
          (.setScissor renderer l b w h)          
          (.setScissorTest renderer true)          
          (.setClearColor renderer background)          
          
          (set! (.-aspect camera) (/ w h))          
          (.updateProjectionMatrix camera)          
          
          (.render renderer scene camera))))))

(comment
  (def cam (.-camera (nth views 1)))
  (.. cam1 -position)
  (.. cam1 -rotation)
  
  (do
    (def cam1 (.-camera (nth views 0)))
    (.. cam1 -position (set 100 100 100))
    #_(set! (.. cam1 -fov) 40)
    #_(set! (.. cam1 -aspect) 0.8)
    
    (.lookAt cam1 (THREE/Vector3. 0 -1 0))
    (.updateProjectionMatrix cam1)
    
    #_(set! (.. cam1 -rotation -x) -1.3)
    )
  
  (set! (.. cam1 -rotation -x) 0)
  
  (.-position (second (.-children (nth  (.-children (.-parent cam1)) 2))))
  
  (.-rotation (nth  (.-children (.-parent cam1)) 2))
  
  (.-rotation (second (.-children (nth  (.-children (.-parent cam1)) 2))))
  
  (do
    (set! views (initiate-views views))
    (init gayo-data/glc gayo-data/scene))
  
  
  )
