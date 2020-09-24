
(ns boat.game
  (:require [clojure.core.reducers :as r] 
            [clojure.string :as str]
            ["three" :as THREE]  
            [promesa.core :as p]
            
            [miracle.soar :as ms]
            
            [gayo.text :refer [set-text!]]
            
            [gayo.sprite :refer [sprite!]]
            [gayo.assets :as assets]
            
            [gayo.log :refer [log!]]            
            [gayo.data :as gayo-data]
            [gayo.state :refer [ensure-state! update-state! state+ state]] 
            [gayo.hooks :as hooks :refer [hook+]]
            [gayo.scene :as scene
             :refer [find-mesh
                     find-mesh-by-name
                     find-meshes-by-data
                     go->card
                     go->top-go
                     go->zone-go]] 
            [gayo.text :refer [set-text!]]
            [gayo.bmfont :refer [default-opts-clj]] 
            [gayo.tweens :as tw]             
            
            [miracle.save] 
            
            [cljs-bean.core :refer [bean ->clj]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defn buttons
  [os]
  (filter #(some-> % .-object .-state .-clicked) os))

(defonce raycaster (THREE/Raycaster.))
(def is-down false)
(def is-moving false)
(def is-moving-far false)
(def is-moving-super-far false)
(def down-pos (THREE/Vector2.))
(def curr-pos (THREE/Vector2.))
(def down-time nil)
(defonce point3d (THREE/Vector3.))
(defonce pos (THREE/Vector3.))

(defn two->three
  [point camera dist]
  (.set point3d (.-x point) (.-y point) 0.5)
  (.normalize (.sub (.unproject point3d camera) (.-position camera)))    
  (let [targetZ (- (.. camera -position -y) dist)
        distance (- (- (/ (- targetZ (.. camera -position -y)) (.-y point3d))))]
    (.add (.copy pos (.-position camera)) (.multiplyScalar point3d distance))
    pos))

(defn find-named
  [name]
  (when-let [scene gayo-data/scene]
    (find-mesh-by-name scene name)))

(defonce ui (THREE/Object3D.))

(defonce main-chars [])

(defonce enemies [])
(defonce enemy nil)
(defonce floor (find-named "Plane"))
(defonce target-ball (find-named "Target"))

(def max-speed 0.03)
(def acc 0.005)

(def slowdown-distance 0.8)
(def snap-distance 0.01)

(def cooldown 500)
(def short-cooldown 0.1)

(def hitted #js [])

(def base-hit-chance 0.5)
(def reaction-time 250) ;; ms

(defonce select-box nil)

(defonce selected nil)

(defn damage
  [obj amount]
  (update-state! obj :hp - amount)
  
  (update-state! obj :stagger #(+ (or % 0) (* 1000 amount)))
  
  (save :nts213)
  #_(macroexpand '(update-state! obj :stagger #(+ (or % 0) (* 1000 amount))))
  (tw/shake-pos! (.-position obj) (THREE/Vector3. 0.1 0 0) #js {:duration 100}))

(defn moving?
  [obj]
  (boolean (some-> (state obj :speed)
                   (> snap-distance))))

(defn alive?
  [obj]
  (boolean (some-> (state obj :hp)
                   (> 0))))

(defn aims-at?
  [o1 o2]
  (= o2 (state o1 :aims-at)))

(defn attack
  [attacker defender]
  (damage defender (state attacker :damage)))

(defn hit-chance
  [attacker defender]
  (* (if (aims-at? defender attacker) 0.25 1)
     (or (state attacker :hit-chance-bonus) 1)
     base-hit-chance))

(defn get-speed
  [o]
  (* (state o :speed)
     (if (state o :stagger)
       (max 0.1 (- 1 (/ (state o :stagger) 1000)))
       1)

     #_(if (state o :aims-at) 0.5 1)))

(comment
  (set-text!
   (first main-chars)
   "0.25"
   #js {:scale 3
        :pos (THREE/Vector3. 0.8 1.8 0)
        :color 0xffffff
        :maxWidth nil})
  
  )

(defn update-hp
  [obj]
  (save :crheoa)
  (let [hp-bar (state obj :hp-bar)
        max-hp-bar (state obj :max-hp-bar)
        tp (.. (state hp-bar :target) -position)
        hp (state (state hp-bar :target) :hp)
        max-hp (state hp-bar :target :max-hp)
        percent (max 0 (/ hp max-hp))]
    (save :aoehnts)
    (.. hp-bar -scale (set percent 0.2 0.2))
    (.. hp-bar -position (set (+ (.-x tp) (* 0.5 (- 1 percent))) (+ (.-y tp) 1.8) (.-z tp)))
    
    (.. max-hp-bar -scale (set 1 0.2 0.2))
    (.. max-hp-bar -position (set (.-x tp) (+ (.-y tp) 1.7999) (.-z tp)))))

(defn give-hp-bar
  [target]
  (save :thnoseahs)
  (if-not (state target :hp-bar)
    (p/let [gs (assets/load-texture "concrete_powder_green.png")
            rs (assets/load-texture "concrete_powder_red.png")]
      (let [hp (sprite! gs)
            max-hp (sprite! rs)]
        
        (.add ui hp)
        (.add ui max-hp)
        
        (ensure-state! hp)
        (ensure-state! max-hp)  
        
        (state+ hp :target target)        
        (state+ max-hp :target target)
        
        (state+ target :hp-bar hp)
        (state+ target :max-hp-bar max-hp)
        
        (hook+ target :second-update :hp #'update-hp)))
    (hook+ target :second-update :hp #'update-hp)))

(defn add-button!
  [f]
  (p/let [texture (assets/load-texture "wool_colored_yellow.png")]
    (let [button (sprite! texture)]
      (.add ui button)
      (ensure-state! button)
      
      (state+ button :clicked f)
      
      (hook+ button :second-update :follow-camera
             (fn [obj]
               (let [{:keys [x y z]} (bean (.. gayo-data/view -camera -position))]
                 (.. obj -position (set (+ (or (.. button -state -offset) 0) x) 10 (- z 10))))))
      
      button)))  


(defn create-line!
  ([start end]
   (create-line! start end #js {:color 0x0000ff})
   )
  ([start end opts]
   (set! (.. start -y) (+ 0.1 (.. start -y)))  
   (set! (.. end -y) (+ 0.1 (.. end -y)))
   
   (let [line-mat (THREE/LineBasicMaterial. opts)
         points #js [start end]
         line-geom (-> (THREE/BufferGeometry.) (.setFromPoints points))
         line (THREE/Line. line-geom line-mat)]
     (.add gayo-data/scene line) 
     line)))

(comment
  (create-line! (THREE/Vector3. 1 1 1)
                (THREE/Vector3. -1 -1 -1))
  
  
  )

(defn enemies-of
  [o]
  (cond ((into #{} main-chars) o)
        , (into #{} (filter some? enemies))
        ((into #{} enemies) o)
        , (into #{} (filter some? main-chars))
        :else #{}))

(defn enemy-of?
  [o possibly-enemy]
  ((enemies-of o) possibly-enemy))

(defn get-status
  [o]
  (cond (state o :drag) "Dragged"
        (moving? o) "Run"
        (state o :aims-at) (str "Aiming: " (let [a (state o :aims-at)]
                                             (or (some-> a .-userData .-name) (.-name a))))
        :else ""))

(defn show-status
  [o]
  (when-not (state o :status)
    (state+ o :status (THREE/Object3D.))
    (.add gayo-data/scene (state o :status)))
  
  (let [status (get-status o)
        status-o (state o :status)
        t (set-text!
           status-o
           status
           #js {:scale 4
                :color 0xffffff
                :maxWidth nil})]
    (.. status-o -position (copy (.-position o)))
    
    (set! (.. status-o -rotation -x) 0.45)
    
    (set! (.. status-o -position -x)
          (+ (.. status-o -position -x) 0))
    (set! (.. status-o -position -y)
          (+ (.. status-o -position -y) 1.8))
    (set! (.. status-o -position -z)
          (- (.. status-o -position -z) 0.5))))

(defn show-hit-chance
  [o]
  (when-not (state o :hit-chance)
    (state+ o :hit-chance (THREE/Object3D.))
    (.add gayo-data/scene (state o :hit-chance)))  
  
  
  (let [hit-chance-o (state o :hit-chance)
        t (set-text!
           hit-chance-o
           (if-let [a (state o :aims-at)]
             (str (* 100 (hit-chance o a)) "")
             "")
           #js {:scale 4
                :color 0xffffff
                :maxWidth nil})]
    (.. hit-chance-o -position (copy (.-position o)))
    
    (set! (.. hit-chance-o -rotation -x) 0.45)
    
    (set! (.. hit-chance-o -position -x)
          (+ (.. hit-chance-o -position -x) -1))
    (set! (.. hit-chance-o -position -y)
          (+ (.. hit-chance-o -position -y) 1.8))))

(defn aim
  [shooter from-pos target-dir]
  (let [raycaster (or (state shooter :raycaster) 
                      (state+ shooter :raycaster (THREE/Raycaster.)))]
    (set! (.. raycaster -far) (state shooter :range)) 
    (.set raycaster from-pos target-dir)                    
    (->> (.intersectObjects raycaster (into-array
                                       (filter #(and (not= ui %)
                                                     (not= shooter %))
                                               (.-children gayo-data/scene)))
                            true)
         (filter #(some? (.-face %))))))

(defn shoot
  [shooter]
  (when-let [target (state shooter :aims-at)]
    (let [from-pos (.clone (.. shooter -position)) 
          target-pos (.clone (.. target -position)) 
          _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y))) 
          _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y))) 
          
          dir (THREE/Vector3.)
          target-dir (.normalize (.subVectors dir target-pos from-pos))
          
          from (.clone from-pos)
          _ (set! (.-x from) (- (+ (rand 0.5) (.-x from)) 0.25))
          _ (set! (.-z from) (- (+ (rand 0.5) (.-z from)) 0.25))
          to (.clone target-pos)
          _ (set! (.-x to) (- (+ (rand 0.5) (.-x to)) 0.25))
          _ (set! (.-z to) (- (+ (rand 0.5) (.-z to)) 0.25))
          line (create-line! from to #js {:color 0xffff00})]
      
      (js/setTimeout 
       #(scene/remove-obj! line)
       250)
      
      (.set raycaster from-pos target-dir)
      (let [intersects (aim shooter from-pos target-dir)]
        (if (some->> (first intersects) .-object (enemy-of? shooter))
          (let [n (rand)
                hc (hit-chance shooter target)]
            (when (< n hc)
              (attack shooter target)))
          (do (println shooter "lost target..." (state shooter :aims-at))
              (state+ shooter :aims-at nil)))))))

(defn try-shoot
  [obj]
  (doseq [enemy (filter alive? (enemies-of obj))
          :when (not (state obj :cooldown))]
    (let [dir (THREE/Vector3.)
          target-pos (.clone (.-position enemy))
          from-pos (.clone (.-position obj))
          _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
          _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
          target-dir (.normalize (.subVectors dir target-pos from-pos))]
      
      (when-let [intersects (seq (aim obj from-pos target-dir))]
        #_(create-line! from-pos target-pos)
        (if (some->> (state obj :aims-at) (alive?))
          (let [target (.-object (first intersects))]
            (state+ obj :cooldown (+ cooldown (rand-int 100)))
            #_(set! (state obj :aims-at) target)
            
            (js/setTimeout #(shoot obj) reaction-time))
          (when (some->> (first intersects) .-object (enemy-of? obj))
            (let [target (.-object (first intersects))]
              (save :crgeroca)
              (state+ obj :cooldown (+ cooldown (rand-int 100)))
              
              (state+ obj :aims-at target)
              
              (js/setTimeout #(shoot obj) reaction-time)))))))
  
  (when-not (state obj :cooldown)
    (when (and (state obj :aims-at)
               (not (alive? (state obj :aims-at))))
      (state+ obj :aims-at nil))
    (state+ obj :cooldown short-cooldown)))

(defn take-aim
  [obj]
  (when (= obj (first main-chars))
    (ms/debug obj {:func #'take-aim}))
  
  (when (and (not (moving? obj))
             (not (state obj :cooldown)))
    (try-shoot obj)))

(defn find-target
  [obj]
  (when-not (some->> (state obj :aims-at) (alive?))
    (let [potential-targets 
          , (for [enemy (filter alive? (enemies-of obj))]
              (let [dir (THREE/Vector3.)
                    target-pos (.clone (.-position enemy))
                    from-pos (.clone (.-position obj))
                    _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
                    _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
                    target-dir (.normalize (.subVectors dir target-pos from-pos))]
                
                (.set raycaster from-pos target-dir)                    
                (when-let [intersects (seq
                                       (->> (.intersectObjects 
                                             raycaster
                                             (into-array
                                              (filter #(and (not= ui %)
                                                            (not= obj %))
                                                      (.-children gayo-data/scene)))
                                             true)
                                            (filter #(some? (.-face %)))))]
                  #_(create-line! from-pos target-pos)
                  (when (some->> (first intersects) .-object (enemy-of? obj))
                    {:pos target-pos
                     :distance (.distanceTo target-pos (.-position obj))}))))
          
          potential-targets (filter some? potential-targets)]
      (if-let [{:keys [pos distance]} (first (sort-by :distance potential-targets))]
        (do
          (if (> distance (- (state obj :range) 0.5))
            (state+ obj :target pos)
            (state+ obj :target (.. obj -position))))))))

(defn find-target-in-ball
  [obj]
  (when-not (or (some->> (state obj :aims-at) (alive?))
                (state obj :drag))
    (let [potential-targets
          , (for [enemy (filter alive? (enemies-of obj))]
              (let [_ (save :htonesa)
                    tb-pos (.. (state obj :target-ball) -position)
                    r (state obj :range)
                    enemy-pos (.. enemy -position)
                    distance (.distanceTo tb-pos enemy-pos)]
                (when (>= r distance) enemy)))
          
          potential-targets
          , (for [enemy potential-targets
                  :when enemy]
              (let [dir (THREE/Vector3.)
                    tb-pos (.. (state obj :target-ball) -position)
                    target-pos (.clone (.-position enemy))
                    from-pos (.clone (.-position obj))
                    _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
                    _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
                    target-dir (.normalize (.subVectors dir target-pos from-pos))]
                
                (.set raycaster from-pos target-dir)                    
                (when-let [intersects (seq
                                       (->> (.intersectObjects 
                                             raycaster
                                             (into-array
                                              (filter #(and (not= ui %)
                                                            (not= obj %))
                                                      (.-children gayo-data/scene)))
                                             true)
                                            (filter #(some? (.-face %)))))]
                  (when (some->> (first intersects) .-object (enemy-of? obj))
                    {:pos target-pos
                     :enemy enemy
                     :distance (.distanceTo target-pos tb-pos)
                     :dist-chars (.distanceTo target-pos from-pos)}))))
          
          potential-targets (filter some? potential-targets)]
      (if-let [{:keys [pos distance dist-chars enemy]} (first (sort-by :distance potential-targets))]
        (when (< distance 1)
          (state+ obj :target pos))))))

(defn reduce-cd
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (state obj :cooldown)]
    (state+ obj :cooldown (- cd dt))
    (when (<= (state obj :cooldown) 0)
      (state+ obj :cooldown nil))))

(defn reduce-stagger
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (state obj :stagger)]
    (state+ obj :stagger (- cd dt))
    (when (<= (state obj :stagger) 0)
      (state+ obj :stagger nil))))

(defn move-to-target
  [obj]
  (if (state obj :aims-at)
    (state+ obj :speed 0)
    (when-let [target-pos (state obj :target)]
      (let [distance (.distanceTo (.. obj -position) target-pos)
            dir (THREE/Vector3.)]
        (state+ obj :aims-at nil)
        
        (save :que-pasta)
        
        (-> (.subVectors dir target-pos (.. obj -position))
            .normalize
            (.multiplyScalar (get-speed obj)))                
        
        (if (< distance snap-distance)
          (do (.. obj -position (copy target-pos))
              (state+ obj
                      :target nil
                      :speed 0))
          (do (state+ obj :speed
                      (if (< distance slowdown-distance)
                        (min max-speed (max 0 (* distance 0.2)))
                        (min max-speed (+ (state obj :speed) acc))))
              (.. obj -position (set (+ (.. obj -position -x) (.-x dir))
                                     (.. obj -position -y)
                                     (+ (.. obj -position -z) (.-z dir))))))
        
        (when-let [target (state obj :target-ball)]
          (when-let [l (state target :line)]
            (.remove gayo-data/scene l))      
          
          (state+ target :line
                  (create-line! (.. obj -position (clone))
                                (.. target -position (clone)))))))))

(defn die
  [obj]
  (when (>= 0 (state obj :hp))
    
    (set! enemies (vec (remove #(= % obj) enemies)))  
    (set! main-chars (vec (remove #(= % obj) main-chars)))  
    

    (println "Deaded" obj)
    (when-let [hp-bar (state obj :hp-bar)]
      (scene/remove-obj! hp-bar)
      (state+ obj :hp-bar nil))
    (when-let [max-hp-bar (state obj :max-hp-bar)]
      (scene/remove-obj! max-hp-bar)
      (state+ obj :max-hp-bar nil))
    (when-let [status (state obj :status)]
      (scene/remove-obj! status)
      (state obj :status) nil)
    (when-let [hit-chance (state obj :hit-chance)]
      (scene/remove-obj! hit-chance)
      (state+ obj :hit-chance nil))
    (set! (.-visible obj) false)
    (hooks/hook- obj)
    #_(scene/remove-obj! obj)))

(def far-range 5)
(def close-range 2)

(defn refresh-target-ball
  [obj]
  (let [tb (state obj :target-ball)]
    (.. tb -scale (set 1 1 1))
    (set! (.. tb -material -opacity)
          (if (state obj :drag)
            0.5
            (if (= selected obj)
              0.3
              0.0)))
    
    (when-let [t (state obj :target)]
      (.. tb -position (copy t)))
    (set! (.. tb -material -map)
          (.. obj -material -map))))

(defn refresh-range-ball
  [obj]
  (let [tb (state obj :range-ball)
        range (* 3 (state obj :range))]
    
    (.. tb -scale (set range 1 range))
    
    (set! (.. tb -material -opacity)
          (if (state obj :drag)
            0.5
            (if (= selected obj)
              0.3
              0.0)))
    
    (.. tb -position (copy (.. obj -position)))
    
    (set! (.. tb -material -map)
          (.. obj -material -map))))

(defn reset-chars!
  []
  (set! main-chars [(find-named "Player")
                    (find-named "Player.001")
                    (find-named "Player.002")])
  
  (set! enemies (find-mesh gayo-data/scene #(some-> (.. % -userData -name) (str/starts-with? "Enemy"))))
  
  (set! floor (find-named "Plane"))
  (set! target-ball (find-named "Target"))
  
  (doseq [c main-chars]
    (ensure-state! c)
    
    (when-not (state c :target-ball)
      (state+ c :target-ball (.clone target-ball))
      (set! (.. (state c :target-ball) -material)
            (.clone (.-material target-ball)))
      
      (.add ui (state c :target-ball)))
    
    (ensure-state! (state c :target-ball))        
    
    (set! (.. (state c :target-ball) -material -blending)
          THREE/AdditiveBlending)    
    (set! (.. (state c :target-ball) -material -depthWrite) false)    
    (set! (.. (state c :target-ball) -material -depthTest) true)
    (set! (.. (state c :target-ball) -material -transparent) true)
    
    (when-not (state c :range-ball)
      (state+ c :range-ball (.clone target-ball))
      (set! (.. (state c :range-ball) -material)
            (.clone (.-material target-ball)))
      
      (.add ui (state c :range-ball)))
    
    
    (ensure-state! (state c :range-ball))        
    
    (set! (.. (state c :range-ball) -material -blending) THREE/AdditiveBlending)    
    (set! (.. (state c :range-ball) -material -depthWrite) false)    
    (set! (.. (state c :range-ball) -material -depthTest) true)
    (set! (.. (state c :range-ball) -material -transparent) true)
    
    (set! (.-visible c) true)
    
    (state+ c :speed 0)
    (state+ c :max-hp 3000)
    (state+ c :hp (state c :max-hp))
    
    (state+ c :damage 1)
    
    (give-hp-bar c)
    
    (hook+ c :update        :move-to-target #'move-to-target)
    (hook+ c :second-update :show-status #'show-status)
    (hook+ c :second-update :refresh-target-ball #'refresh-target-ball)
    (hook+ c :second-update :refresh-range-ball  #'refresh-range-ball)
    (hook+ c :second-update :show-hit-chance #'show-hit-chance)
    (hook+ c :update :die #'die)
    (hook+ c :update :take-aim #'take-aim)
    (hook+ c :update :find-target #'find-target-in-ball)
    (hook+ c :update :reduce-cd #'reduce-cd)
    (hook+ c :update :reduce-stagger #'reduce-stagger))
  
  (state+ (first main-chars) :range close-range)
  (state+ (first main-chars) :hit-chance-bonus 2)
  (state+ (first main-chars) :damage 3)
  (state+ (second main-chars) :range far-range)
  (state+ (get main-chars 2) :range far-range)
  
  (set! selected (first main-chars))
  
  (doseq [enemy enemies]
    (ensure-state! enemy)
    
    (state+ enemy :range far-range)
    
    (set! (.-visible enemy) true)
    
    (hook+ enemy :update :show-status #'show-status)
    
    (state+ enemy :speed 0)
    (state+ enemy :max-hp 15)
    (state+ enemy :hp (state enemy :max-hp))
    (state+ enemy :damage 0.5)
    
    (give-hp-bar enemy)
    
    (hook+ enemy :second-update :show-hit-chance #'show-hit-chance)
    (hook+ enemy :update :move-to-target #'move-to-target)
    (hook+ enemy :update :find-target #'find-target)
    (hook+ enemy :update :die #'die)
    (hook+ enemy :update :take-aim #'take-aim)
    (hook+ enemy :update :reduce-cd #'reduce-cd)
    (hook+ enemy :update :reduce-stagger #'reduce-stagger)))

(comment
  
  (reset-chars!)

  )

(defn init
  [scene loaded-gltf conf-k]
  
  (set! ui (THREE/Object3D.))
  (.add scene ui)
  
  (set! select-box (sprite! nil))
  
  (.add ui select-box)
  
  (ensure-state! select-box)
  
  (.. select-box -scale (set 1.1 1.1 1.1))
  
  (hook+
   select-box
   :second-update
   :follow-selected
   (fn [obj]
     (let [{:keys [x y z]} (bean (.. gayo-data/view -camera -position))]
       (.. obj -position
           (set (+ (or (state obj :offset) 0) x)
                9.999
                (- z 9.999))))))
  
  (let [light (THREE/AmbientLight. 0xffffff)]    
    (save :add-light1)
    (set! (.. light -intensity) 2)
    (.add scene light))
  
  (reset-chars!)
  
  (p/then (add-button! (fn []
                         (set! selected (first main-chars))
                         (state+ select-box :offset -2)))
          (fn [b]
            (def b1 b)
            (set! (.. b1 -material -map) (.. (get main-chars 0) -material -map))
            (state+ b1 :offset -2)))
  (p/then (add-button! (fn [] (set! selected (second main-chars))
                         (set! (.. select-box -state -offset) 0)))
          (fn [b]
            (def b2 b)
            (set! (.. b2 -material -map) (.. (get main-chars 1) -material -map))
            (state+ b2 :offset 0)))
  (p/then (add-button! (fn []
                         (set! selected (get main-chars 2))
                         (set! (.. select-box -state -offset) 2)))
          (fn [b]
            (def b3 b)
            (set! (.. b3 -material -map) (.. (get main-chars 2) -material -map))
            (state+ b3 :offset 2)))
  
  

  )

(comment
  (.-children (.-scene gayo-data/loaded-scene))
  
  
  
  (bean (find-named "Light"))
  
  
  )

(defn update!
  [_ camera _]
  (ms/debug (first main-chars) {:func #'update!})
  
  (when-not selected
    (set! selected (first (filter alive? main-chars))))
  
  (def camera camera)
  
  (.. camera -rotation)
  
  (when-let [c (and is-down #_ selected (first (filter #(state % :drag) main-chars)))]
    (.setFromCamera raycaster curr-pos camera)
    (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
      (set! hitted intersects)
      (when-not (seq (buttons intersects))
        (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
          (let [tb (state c :target-ball)
                target (state+ c :target (.clone (.-point hit-floor)))]
            (.. tb -position (copy target))
            
            (when-let [l (state tb :line)]
              (.remove gayo-data/scene l))      
            
            (state+ tb :line
                    (create-line! (.. c -position (clone))
                                  (.. tb -position (clone)))))))))
  
  
  
  (when-let [cam-target selected #_ (first (filter #(> (.. % -state -hp) 0) main-chars))]
    (.. camera -position (set                          (+ (.. cam-target -position -x) 0)
                                                       30
                                                       (+ (.. cam-target -position -z) 20)))
    
    (.. camera (lookAt (.-position cam-target))))
  
  )

(defn start
  [point scene camera]
  
  (set! is-moving false)
  (set! is-moving-far false)
  (set! is-moving-super-far false)
  (set! is-down true)
  (.copy down-pos point)
  (.copy curr-pos point)
  
  (set! down-time gayo-data/last-time)
  
  (doseq [c main-chars]
    (state+ c :drag false))
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
    (set! hitted intersects)
    (when-not (seq (buttons hitted))
      (let [to-move (if-let [hit-main-char (first (filter #((into #{} main-chars) (.-object %)) hitted))]
                      (.-object hit-main-char)
                      selected)]
        (.. (state to-move :target-ball)
            -position (copy (.-position to-move)))
        (state+ to-move :drag true)
        (state+ to-move :aims-at nil))))
  
  (update! scene camera 0))

(defn frame
  [scene camera]
  
  )

(defn move
  [point scene camera]
  (.copy curr-pos point) 
  
  (when (< 0.0005 (.distanceToSquared down-pos point))
    (set! is-moving true))
  
  (when (< 0.01 (.distanceToSquared down-pos point))
    (set! is-moving-far true))
  
  (when (< 0.5 (.distanceToSquared down-pos point))
    (set! is-moving-super-far true)))

(defn release
  [point scene camera]
  (log! "release")
  
  (when-not (first (filter #(state % :drag) main-chars))
    (do (.setFromCamera raycaster curr-pos camera)
        (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
          (set! hitted intersects)
          
          (if-let [hit-button (first (buttons intersects))]
            ((.. hit-button -object -state -clicked) point)
            
            (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
              (save :eouantsh)
              (state+ selected :speed 0)
              (state+ selected :target (.clone (.-point hit-floor))))))))
  
  (hooks/run-hooks! :release)
  (hooks/run-hooks! :click)
  
  (doseq [c main-chars]
    (state+ c :drag false))
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children scene))]
    
    )
  
  (set! is-moving false)
  (set! is-down false)) 
