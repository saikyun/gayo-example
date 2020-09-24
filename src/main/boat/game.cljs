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
            [gayo.state :refer [ensure-state! upd! state]] 
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
  (set! (.. obj -state -hp) (- (.. obj -state -hp) amount))
  (set! (.. obj -state -stagger) (+ (or (.. obj -state -stagger) 0)
                                    (* 1000 amount)))
  (tw/shake-pos! (.-position obj) (THREE/Vector3. 0.1 0 0) #js {:duration 100}))

(defn is-moving?
  [obj]
  (boolean (some-> (.. obj -state -speed)
                   (> snap-distance))))

(defn alive?
  [obj]
  (boolean (some-> (.. obj -state -hp)
                   (> 0))))

(defn aims-at?
  [o1 o2]
  (= o2 (.. o1 -state -aimsAt)))

(defn attack
  [attacker defender]
  (damage defender (.. attacker -state -damage)))

(defn hit-chance
  [attacker defender]
  (* (if (aims-at? defender attacker) 0.25 1)
     (or (.. attacker -state -hitChanceBonus) 1)
     base-hit-chance))

(defn get-speed
  [o]
  (* (.. o -state -speed)
     (if (.. o -state -stagger)
       (max 0.1 (- 1 (/ (.. o -state -stagger) 1000)))
       1)

     #_(if (.. o -state -aimsAt) 0.5 1)))

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
  (let [hp-bar (.. obj -state -hpBar)
        max-hp-bar (.. obj -state -maxHpBar)
        tp (.. hp-bar -state -target -position)
        hp (.. hp-bar -state -target -state -hp)
        max-hp (.. hp-bar -state -target -state -maxHp)
        percent (max 0 (/ hp max-hp))]
    (save :aoehnts)
    (.. hp-bar -scale (set percent 0.2 0.2))
    (.. hp-bar -position (set (+ (.-x tp) (* 0.5 (- 1 percent))) (+ (.-y tp) 1.8) (.-z tp)))
    
    (.. max-hp-bar -scale (set 1 0.2 0.2))
    (.. max-hp-bar -position (set (.-x tp) (+ (.-y tp) 1.7999) (.-z tp)))))

(defn give-hp-bar
  [target]
  (if-not (.. target -state -hpBar)
    (p/let [gs (assets/load-texture "concrete_powder_green.png")
            rs (assets/load-texture "concrete_powder_red.png")]
      (let [hp (sprite! gs)
            max-hp (sprite! rs)]
        
        (.add ui hp)
        (.add ui max-hp)
        
        (ensure-state! hp)
        (ensure-state! max-hp)  
        (set! (.. hp -state -target) target)
        
        (set! (.. target -state -hpBar) hp)
        (set! (.. target -state -maxHpBar) max-hp)
        
        (hook+ target :second-update :hp #'update-hp)))
    (hook+ target :second-update :hp #'update-hp)))

(defn add-button!
  [f]
  (p/let [texture (assets/load-texture "wool_colored_yellow.png")]
    (let [button (sprite! texture)]
      (.add ui button)
      (ensure-state! button)
      
      (set! (.. button -state -clicked) f)
      
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
  (cond (.. o -state -drag) "Dragged"
        (is-moving? o) "Run"
        (.. o -state -aimsAt) (str "Aiming: " (let [a (.. o -state -aimsAt)]
                                                (or (some-> a .-userData .-name) (.-name a))))
        :else ""))

(defn show-status
  [o]
  (when-not (.. o -state -status)
    (set! (.. o -state -status) (THREE/Object3D.))
    (.add gayo-data/scene (.. o -state -status)))
  
  (let [status (get-status o)
        status-o (.. o -state -status)
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
  (when-not (.. o -state -hitChance)
    (set! (.. o -state -hitChance) (THREE/Object3D.))
    (.add gayo-data/scene (.. o -state -hitChance)))  
  
  
  (let [hit-chance-o (.. o -state -hitChance)
        t (set-text!
           hit-chance-o
           (if-let [a (.. o -state -aimsAt)]
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
  (when-not (.. shooter -state -raycaster)
    (set! (.. shooter -state -raycaster) (THREE/Raycaster.)))
  
  (let [raycaster (.. shooter -state -raycaster)]
    (set! (.. raycaster -far) (.. shooter -state -range)) 
    (.set raycaster from-pos target-dir)                    
    (->> (.intersectObjects raycaster (into-array
                                       (filter #(and (not= ui %)
                                                     (not= shooter %))
                                               (.-children gayo-data/scene)))
                            true)
         (filter #(some? (.-face %))))))

(defn shoot
  [shooter]
  (when-let [target (.. shooter -state -aimsAt)]
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
          (do (println shooter "lost target..." (.. shooter -state -aimsAt))
              (set! (.. shooter -state -aimsAt) nil)))))))

(defn try-shoot
  [obj]
  (doseq [enemy (filter alive? (enemies-of obj))
          :when (not (.. obj -state -cooldown))]
    (let [dir (THREE/Vector3.)
          target-pos (.clone (.-position enemy))
          from-pos (.clone (.-position obj))
          _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
          _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
          target-dir (.normalize (.subVectors dir target-pos from-pos))]
      
      (when-let [intersects (seq (aim obj from-pos target-dir))]
        #_(create-line! from-pos target-pos)
        (if (some->> (.. obj -state -aimsAt) (alive?))
          (let [target (.-object (first intersects))]
            (set! (.. obj -state -cooldown) (+ cooldown (rand-int 100)))
            #_(set! (.. obj -state -aimsAt) target)
            
            (js/setTimeout #(shoot obj) reaction-time))
          (when (some->> (first intersects) .-object (enemy-of? obj))
            (let [target (.-object (first intersects))]
              (set! (.. obj -state -cooldown) (+ cooldown (rand-int 100)))
              (set! (.. obj -state -aimsAt) target)
              
              (js/setTimeout #(shoot obj) reaction-time)))))))
  
  (when-not (.. obj -state -cooldown)
    (when (and (.. obj -state -aimsAt)
               (not (alive? (.. obj -state -aimsAt))))
      (set! (.. obj -state -aimsAt) nil))
    (set! (.. obj -state -cooldown) short-cooldown)))

(defn take-aim
  [obj]
  (ms/debug obj {:func #'take-aim})
  (when (not (.. obj -state -cooldown))
    (try-shoot obj)))

(defn find-target
  [obj]
  (when-not (some->> (.. obj -state -aimsAt) (alive?))
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
          (if (> distance (- (.. obj -state -range) 0.5))
            (set! (.. obj -state -target) pos)
            (set! (.. obj -state -target)
                  (.. obj -position))))))))

(defn find-target-in-ball
  [obj]
  (when-not (or (some->> (.. obj -state -aimsAt) (alive?))
                (.. obj -state -drag))
    (let [potential-targets
          , (for [enemy (filter alive? (enemies-of obj))]
              (let [tb-pos (.. obj -state -targetBall -position)
                    r (.. obj -state -range)
                    enemy-pos (.. enemy -position)
                    distance (.distanceTo tb-pos enemy-pos)]
                (when (>= r distance)
                  enemy)))
          
          potential-targets
          , (for [enemy potential-targets
                  :when enemy]
              (let [dir (THREE/Vector3.)
                    tb-pos (.. obj -state -targetBall -position)
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
          (set! (.. obj -state -target) pos))))))

(defn reduce-cd
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (.. obj -state -cooldown)]
    (set! (.. obj -state -cooldown)
          (- cd dt))
    (when (<= (.. obj -state -cooldown) 0)
      (set! (.. obj -state -cooldown) nil))))

(defn reduce-stagger
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (.. obj -state -stagger)]
    (set! (.. obj -state -stagger)
          (- cd dt))
    (when (<= (.. obj -state -stagger) 0)
      (set! (.. obj -state -stagger) nil))))

(defn move-to-target
  [obj]
  (if (.. obj -state -aimsAt)
    (set! (.. obj -state -speed) 0)
    (when-let [target-pos (.. obj -state -target)]
      (let [distance (.distanceTo (.. obj -position) target-pos)
            dir (THREE/Vector3.)]
        (set! (.. obj -state -aimsAt) nil)    
        
        (save :que-pasta)
        
        (-> (.subVectors dir target-pos (.. obj -position))
            .normalize
            (.multiplyScalar (get-speed obj)))                
        
        (if (< distance snap-distance)
          (do (.. obj -position (copy target-pos))
              (set! (.. obj -state -target) nil)
              (set! (.. obj -state -speed) 0))
          (do (set! (.. obj -state -speed)
                    (if (< distance slowdown-distance)
                      (min max-speed (max 0 (* distance 0.2)))
                      (min max-speed (+ (.. obj -state -speed) acc))))
              (.. obj -position (set (+ (.. obj -position -x) (.-x dir))
                                     (.. obj -position -y)
                                     (+ (.. obj -position -z) (.-z dir))))))
        
        (when-let [target (.. obj -state -targetBall)]
          (when-let [l (.. target -state -line)]
            (.remove gayo-data/scene l))      
          
          (set! (.. target -state -line)
                (create-line! (.. obj -position (clone))
                              (.. target -position (clone)))))))))

(defn die
  [obj]
  (when (>= 0 (.. obj -state -hp))
    
    (set! enemies (vec (remove #(= % obj) enemies)))  
    (set! main-chars (vec (remove #(= % obj) main-chars)))  
    

    (println "Deaded" obj)
    (when-let [hp-bar (.. obj -state -hpBar)]
      (scene/remove-obj! hp-bar)
      (set! (.. obj -state -hpBar) nil))
    (when-let [max-hp-bar (.. obj -state -maxHpBar)]
      (scene/remove-obj! max-hp-bar)
      (set! (.. obj -state -maxHpBar) nil))
    (when-let [status (.. obj -state -status)]
      (scene/remove-obj! status)
      (set! (.. obj -state -status) nil))
    (when-let [hit-chance (.. obj -state -hitChance)]
      (scene/remove-obj! hit-chance)
      (set! (.. obj -state -hitChance) nil))
    (set! (.-visible obj) false)
    (hooks/hook- obj)
    #_(scene/remove-obj! obj)))

(def far-range 5)
(def close-range 2)

(comment
  (bean (.. (first main-chars) -state -targetBall))

  (.. (first main-chars) -state -targetBall -scale (set 4 1 4))
  
  (set! (.. (first main-chars) -state -targetBall -material -transparent)
        true)  
  
  
  (.. (first main-chars) -state -targetBall -position (set 1 1 1))
  )

(defn refresh-target-ball
  [obj]
  (let [tb (.. obj -state -targetBall)]
    (.. tb -scale (set 1 1 1))
    (set! (.. tb -material -opacity)
          (if (.. obj -state -drag)
            0.5
            (if (= selected obj)
              0.3
              0.0)))
    (when-let [t (and (not (.. obj -state -drag))
                      (.. obj -state -target))]
      (.. tb -position (copy t)))
    (set! (.. tb -material -map)
          (.. obj -material -map))))

(defn refresh-range-ball
  [obj]
  (let [tb (.. obj -state -rangeBall)
        range (* 3 (.. obj -state -range))]
    (.. tb -scale (set range 1 range))
    
    (set! (.. tb -material -opacity)
          (if (.. obj -state -drag)
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
    
    (when-not (.. c -state -targetBall)
      (set! (.. c -state -targetBall) (.clone target-ball))
      (set! (.. c -state -targetBall -material)
            (.clone (.-material target-ball)))
      
      (.add ui (.. c -state -targetBall)))
    
    (ensure-state! (.. c -state -targetBall))        
    
    (set! (.. c -state -targetBall -material -blending)
          THREE/AdditiveBlending)    
    (set! (.. c -state -targetBall -material -depthWrite) false)    
    (set! (.. c -state -targetBall -material -depthTest) true)
    (set! (.. c -state -targetBall -material -transparent) true)
    
    (when-not (.. c -state -rangeBall)
      (set! (.. c -state -rangeBall) (.clone target-ball))
      (set! (.. c -state -rangeBall -material)
            (.clone (.-material target-ball)))
      
      (.add ui (.. c -state -rangeBall)))
    
    
    (ensure-state! (.. c -state -rangeBall))        
    
    (set! (.. c -state -rangeBall -material -blending)
          THREE/AdditiveBlending)    
    (set! (.. c -state -rangeBall -material -depthWrite) false)    
    (set! (.. c -state -rangeBall -material -depthTest) true)
    (set! (.. c -state -rangeBall -material -transparent) true)
    
    
    (set! (.-visible c) true)
    
    (set! (.. c -state -speed) 0)
    (set! (.. c -state -maxHp) 3000)
    (set! (.. c -state -hp) (.. c -state -maxHp))  
    
    (set! (.. c -state -damage) 1)
    
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
  
  (set! (.. (first main-chars) -state -range) close-range)
  (set! (.. (first main-chars) -state -hitChanceBonus) 2)
  (set! (.. (first main-chars) -state -damage) 3)
  (set! (.. (second main-chars) -state -range) far-range)
  (set! (.. (get main-chars 2) -state -range) far-range)
  
  (set! selected (first main-chars))
  
  (doseq [enemy enemies]
    (ensure-state! enemy)
    
    (set! (.. enemy -state -range) far-range)
    
    (set! (.-visible enemy) true)
    
    (hook+ enemy :update :show-status #'show-status)
    
    (set! (.. enemy -state -speed) 0)
    (set! (.. enemy -state -maxHp) 15)
    (set! (.. enemy -state -hp) (.. enemy -state -maxHp))
    
    (set! (.. enemy -state -damage) 0.5)
    
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
           (set (+ (or (.. obj -state -offset) 0) x)
                9.999
                (- z 9.999))))))
  
  (let [light (THREE/AmbientLight. 0xffffff)]    
    (save :add-light1)
    (set! (.. light -intensity) 2)
    (.add scene light))
  
  (reset-chars!)
  
  (p/then (add-button! (fn []
                         (set! selected (first main-chars))
                         (set! (.. select-box -state -offset) -2)))
          (fn [b]
            (def b1 b)
            (set! (.. b1 -material -map) (.. (get main-chars 0) -material -map))
            (set! (.. b1 -state -offset) -2)))
  (p/then (add-button! (fn [] (set! selected (second main-chars))
                         (set! (.. select-box -state -offset) 0)))
          (fn [b]
            (def b2 b)
            (set! (.. b2 -material -map) (.. (get main-chars 1) -material -map))
            (set! (.. b2 -state -offset) 0)))
  (p/then (add-button! (fn []
                         (set! selected (get main-chars 2))
                         (set! (.. select-box -state -offset) 2)))
          (fn [b]
            (def b3 b)
            (set! (.. b3 -material -map) (.. (get main-chars 2) -material -map))
            (set! (.. b3 -state -offset) 2)))
  
  

  )

(comment
  (.-children (.-scene gayo-data/loaded-scene))
  
  
  
  (bean (find-named "Light"))
  
  
  )

(defn update!
  [_ camera _]
  (when-not selected
    (set! selected (first (filter alive? main-chars))))
  
  (def camera camera)
  
  (.. camera -rotation)
  
  (when-let [c (and is-down #_ selected (first (filter #(.. % -state -drag) main-chars)))]
    (.setFromCamera raycaster curr-pos camera)
    (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
      (set! hitted intersects)
      (when-not (seq (buttons intersects))
        (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
          (let [target (.. c -state -targetBall)]
            (.. target -position (copy (.-point hit-floor)))
            
            (when-let [l (.. target -state -line)]
              (.remove gayo-data/scene l))      
            
            (set! (.. target -state -line)
                  (create-line! (.. c -position (clone))
                                (.. target -position (clone)))))))))
  
  
  
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
    (set! (.. c -state -drag) false))
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
    (set! hitted intersects)
    (when-not (seq (buttons hitted))
      (let [to-move (if-let [hit-main-char (first (filter #((into #{} main-chars) (.-object %)) hitted))]
                      (.-object hit-main-char)
                      selected)]
        (.. to-move -state -targetBall -position (copy (.-position to-move)))
        (set! (.. to-move -state -drag) true)
        (set! (.. to-move -state -aimsAt) nil))))
  
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
  
  (if-let [c (first (filter #(.. % -state -drag) main-chars))]
    (do (.setFromCamera raycaster curr-pos camera)
        (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
          (set! hitted intersects)
          
          (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
            (save :eouantsh)
            (set! (.. c -state -speed) 0)
            (set! (.. c -state -target) (.clone (.-point hit-floor))))))

    (do (.setFromCamera raycaster curr-pos camera)
        (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
          (set! hitted intersects)
          
          (if-let [hit-button (first (buttons intersects))]
            ((.. hit-button -object -state -clicked) point)
            
            (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
              (save :eouantsh)
              (set! (.. selected -state -speed) 0)
              (set! (.. selected -state -target)
                    (.clone (.-point hit-floor))))))))
  
  (hooks/run-hooks! :release)
  (hooks/run-hooks! :click)
  
  (doseq [c main-chars]
    (set! (.. c -state -drag) false))
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children scene))]
    
    )
  
  (set! is-moving false)
  (set! is-down false)) 
