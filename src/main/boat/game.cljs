(ns boat.game
  (:require [clojure.core.reducers :as r] 
            [clojure.string :as str]             
            ["three" :as THREE]  
            [promesa.core :as p]

            [gayo.text :refer [set-text!]]
            
            [gayo.sprite :refer [sprite!]]
            [gayo.assets :as assets]
            
            [gayo.log :refer [log!]]            
            [gayo.data :as gayo-data]
            [gayo.state :refer [ensure-state!]] 
            [gayo.hooks :as hooks :refer [hook+]]
            [gayo.scene :as scene
             :refer [find-mesh-by-name
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

(def main-chars (into [] (filter some? [(find-named "Player")
                                        (find-named "Player.001")])))
(def enemy (find-named "Enemy"))
(def floor (find-named "Plane"))
(def target (find-named "Target"))

(def speed 0.05)

(def max-speed 0.08)
(def acc 0.005)

(def slowdown-distance 0.8)
(def snap-distance 0.01)

(def cooldown 1000)
(def short-cooldown 0.1)

(def hitted #js [])

(def base-hit-chance 0.5)
(def reaction-time 250) ;; ms

(defn damage
  [obj amount]
  (set! (.. obj -state -hp) (- (.. obj -state -hp) amount)))

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
     base-hit-chance))

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
      
      (hook+ target :second-update :hp #'update-hp))))


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
  
  
  enemy
  )

(defn enemies
  [o]
  (cond ((into #{} main-chars) o)
        , (into #{} (filter some? [enemy]))
        (= o enemy)
        , (into #{} main-chars)
        :else #{}))

(defn enemy-of?
  [o possibly-enemy]
  ((enemies o) possibly-enemy))

(defn get-status
  [o]
  (cond (is-moving? o) "Run"
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
          (+ (.. status-o -position -x) 1))
    (set! (.. status-o -position -y)
          (+ (.. status-o -position -y) 1.8))))

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
      (let [intersects (->> (.intersectObjects raycaster (into-array
                                                          (filter #(and (not= ui %)
                                                                        (not= shooter %))
                                                                  (.-children gayo-data/scene)))
                                               true)
                            (filter #(some? (.-face %))))]
        (if (some->> (first intersects) .-object (enemy-of? shooter))
          (let [hc (hit-chance shooter target)]
            (save :hntoae)
            (set-text!
             shooter
             (str hc)
             #js {:scale 3
                  :pos (THREE/Vector3. 0.8 1.8 0)
                  :color 0xffffff
                  :maxWidth nil})
            (when (> (rand) hc)
              (attack shooter target)))
          (set! (.. shooter -state -aimsAt) nil))))))

(comment (enemies (first main-chars)))

(defn try-shoot
  [obj]
  (doseq [enemy (filter alive? (enemies obj))]
    (let [dir (THREE/Vector3.)
          target-pos (.clone (.-position enemy))
          from-pos (.clone (.-position obj))
          _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
          _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
          target-dir (.normalize (.subVectors dir target-pos from-pos))]
      
      (.set raycaster from-pos target-dir)
      (let [intersects 
            (->> (.intersectObjects raycaster (into-array
                                               (filter #(and (not= ui %)
                                                             (not= obj %))
                                                       (.-children gayo-data/scene)))
                                    true)
                 (filter #(some? (.-face %))))]
        (if (some->> (.. obj -state -aimsAt) (alive?))
          (let [target (.-object (first intersects))]
            (set! (.. obj -state -cooldown) cooldown)
            (set! (.. obj -state -aimsAt) target)
            
            (js/setTimeout #(shoot obj) reaction-time))
          (when (some->> (first intersects) .-object (enemy-of? obj))
            (println "enemy!" obj (.-object (first intersects)))
            (let [target (.-object (first intersects))]
              (set! (.. obj -state -cooldown) cooldown)
              (set! (.. obj -state -aimsAt) target)
              (println "enemy!" obj (.. obj -state -aimsAt))
              
              (js/setTimeout #(shoot obj) reaction-time)))))))
  
  (when-not (.. obj -state -cooldown)
    (set! (.. obj -state -cooldown) short-cooldown)))

(defn take-aim
  [obj]
  (when (and (not (is-moving? obj))
             (not (.. obj -state -cooldown)))
    (try-shoot obj)))

(defn reduce-cd
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (.. obj -state -cooldown)]
    (set! (.. obj -state -cooldown)
          (- cd dt))
    (when (<= (.. obj -state -cooldown) 0)
      (set! (.. obj -state -cooldown) nil))))

(defn move-to-target
  [obj]
  (when-let [target-pos (.. obj -state -target)]
    (set! (.. obj -state -aimsAt) nil)

    (let [distance (.distanceTo (.. obj -position) target-pos)
          dir (THREE/Vector3.)]
      
      (-> (.subVectors dir target-pos (.. obj -position))
          .normalize
          (.multiplyScalar (.. obj -state -speed)))                
      
      (if (< distance snap-distance)
        (do (.. obj -position (copy target-pos))
            (set! (.. obj -state -target) nil)
            (set! (.. obj -state -speed) 0))
        (do (set! (.. obj -state -speed)
                  (if (< distance slowdown-distance)
                    (min speed (max (* speed 0.2) (* distance 0.2)))
                    (min max-speed (+ (.. obj -state -speed) acc))))
            (.. obj -position (set (+ (.. obj -position -x) (.-x dir))
                                   (.. obj -position -y)
                                   (+ (.. obj -position -z) (.-z dir))))))
      
      (when-let [l (.. target -state -line)]
        (.remove gayo-data/scene l))      
      
      (set! (.. target -state -line)
            (create-line! (.. obj -position (clone))
                          (.. target -position (clone))))
      
      (save :que-pasta))))

(defn die
  [obj]
  (when (>= 0 (.. obj -state -hp))
    (println "Deaded")
    (scene/remove-obj! obj)))

(defn reset-chars!
  []
  
  (set! main-chars [(find-named "Player")
                    (find-named "Player.001")])
  
  (set! enemy (find-named "Enemy"))
  (set! floor (find-named "Plane"))
  (set! target (find-named "Target"))
  
  (doseq [c main-chars]
    (ensure-state! c)
    
    (set! (.. c -state -maxHp) 7)  
    (set! (.. c -state -hp) (.. c -state -maxHp))  
    
    (set! (.. c -state -damage) 1)
    
    (give-hp-bar c)
    
    (hook+ c :update :move-to-target #'move-to-target)
    (hook+ c :second-update :show-status #'show-status)
    (hook+ c :update :die #'die)
    (hook+ c :update :take-aim #'take-aim)
    (hook+ c :update :reduce-cd #'reduce-cd))
  
  (hook+ enemy :update :show-status #'show-status)
  
  (set! (.. enemy -state -maxHp) 7)  
  (set! (.. enemy -state -hp) (.. enemy -state -maxHp))
  
  (set! (.. enemy -state -damage) 0.5)
  
  (give-hp-bar enemy)
  
  
  (hook+ enemy :update :die #'die)
  
  (hook+ enemy     :update :take-aim #'take-aim)
  
  
  (hook+ enemy :update :reduce-cd #'reduce-cd))

(comment
  (reset-chars!)
  )

(defn init
  [scene loaded-gltf conf-k]
  
  (set! ui (THREE/Object3D.))
  (.add scene ui)
  
  (let [light (THREE/AmbientLight. 0xffffff)]    
    (save :add-light1)
    (set! (.. light -intensity) 2)
    (.add scene light))
  
  (reset-chars!)
  )

(comment
  (.-children (.-scene gayo-data/loaded-scene))
  
  
  
  (bean (find-named "Light"))
  
  
  )

(defn update!
  [_ camera _]
  (def camera camera)
  
  (.. camera -rotation)
  
  (when-let [c (and is-down (first (filter #(.. % -state -drag) main-chars)))]
    (.setFromCamera raycaster curr-pos camera)
    (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
      (set! hitted intersects)
      (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
        (.. target -position (copy (.-point hit-floor)))
        (.. target -scale (set 2 2 2))
        
        (when-let [l (.. target -state -line)]
          (.remove gayo-data/scene l))      
        
        (set! (.. target -state -line)
              (create-line! (.. c -position (clone))
                            (.. target -position (clone)))))))
  
  
  
  (when-let [cam-target (first (filter #(> (.. % -state -hp) 0) main-chars))]
    (.. camera -position (set
                          (+ (.. cam-target -position -x) 0)
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
    (when-let [hit-main-char (first (filter #((into #{} main-chars) (.-object %)) hitted))]
      (set! (.. (.-object hit-main-char) -state -drag) true)))
  
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
  
  (.. target -scale (set 1 1 1))
  
  (when-let [c (first (filter #(.. % -state -drag) main-chars))]
    (.setFromCamera raycaster curr-pos camera)
    (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
      (set! hitted intersects)
      
      (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
        (save :eouantsh)
        (set! (.. c -state -speed) 0)
        (set! (.. c -state -target) (.clone (.-point hit-floor))))))
  
  (hooks/run-hooks! :release)
  (hooks/run-hooks! :click)
  
  (doseq [c main-chars]
    (set! (.. c -state -drag) false))
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children scene))]
    
    )
  
  (set! is-moving false)
  (set! is-down false)) 
