(ns boat.game
  (:require [clojure.core.reducers :as r] 
            [clojure.string :as str]
            ["three" :as THREE]  
            [promesa.core :as p]
            
            [miracle.soar :as ms]
            
            [gayo.animation :as anim]
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
                     find-mesh-name-starts-with
                     go->card
                     go->top-go
                     go->zone-go]] 
            [gayo.text :refer [set-text!]]
            [gayo.bmfont :refer [default-opts-clj]] 
            [gayo.tweens :as tw]             
            
            [miracle.save] 
            
            [cljs-bean.core :refer [bean ->clj]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defn clickables
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
(ensure-state! ui)

(defonce main-chars [])

(defonce enemies [])
(defonce enemy nil)
(defonce floor (find-named "Plane"))
(defonce target-ball (find-named "Target"))
(defonce throwable-projectile nil)
(defonce explosion nil)
(defonce shield nil)
(defonce blue-explosion nil)

(def dragging nil)

(defn point-on-floor
  ([point]
   (point-on-floor point (.. gayo-data/view -camera)))
  ([point camera]
   (.setFromCamera raycaster point camera)
   (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
     (when-not (first (clickables intersects))
       (when-let [hit-floor (first (filter #(= (.-object %) floor) intersects))]
         (.-point hit-floor))))))

(def max-speed 0.03)
(def acc 0.005)

(def slowdown-distance 0.8)
(def snap-distance 0.01)
(def adjust-distance 0.5)

(def cooldown 500)
(def short-cooldown 0.1)

(def hitted #js [])

(def base-hit-chance 0.5)
(def reaction-time 250) ;; ms

(defonce select-box nil)

(defonce selected nil)

(defn select
  [button]
  (set! selected (state button :character))
  (state+ select-box :offset (state button :offset))
  (.. gayo-data/view -camera -rotation (set -0.9827 0 0))
  (.. gayo-data/view -camera -position (set (+ (.. selected -position -x) 0)
                                            20
                                            (+ (.. selected -position -z) 12))))

(defn select-character
  [character]
  (set! selected character)
  (state+ select-box :offset (state character :button :offset))
  #_(.. gayo-data/view -camera -position (set (+ (.. selected -position -x) 0)
                                              20
                                              (+ (.. selected -position -z) 12))))

(declare find-target damage)

(defn moving?
  [obj]
  (boolean (some-> (state obj :speed)
                   (> snap-distance))))

(comment
  (map #(state %) main-chars)
  )

(defn explosion-damage
  [o]
  (doseq [c (concat enemies main-chars)]
    (when (or (not (state o :damaged)) (not ((state o :damaged) c)))
      (when (< (.distanceTo (.-position o) (.-position c)) (state o :range))
        (damage c (state o :damage) o)
        (update-state! o :damaged #(into #{} (conj % c)))))))

(defn explode!
  ([pos]
   (explode! pos nil))
  ([pos {:keys [range   height       damage model]
         :or   {range 1 height range damage 10 model explosion} :as opts}]
   (let [o (.clone model)]
     (ensure-state! o)
     (.. o -position (copy pos))
     (.. ui (add o))
     (state+ o :damage damage)
     (state+ o :range range)
     
     (hook+ o :update :damage #'explosion-damage)
     
     (.. o -scale (set (* 0.9 range)))
     
     (js/setTimeout (fn [] (scene/remove-obj! o)) 500)
     (tw/tween-scale! (.-scale o)
                      (THREE/Vector3. (* 0.9 range)
                                      (* 0.9 height)
                                      (* 0.9 range))
                      #js {:duration 500
                           :easing tw/bounce})
     
     o)))

(defn inc-cant-aim
  [o]
  (save :TNhsaoe)
  (update-state! o :cant-aim inc))

(defn dec-cant-aim
  [o]
  (when (>= 0 (update-state! o :cant-aim dec))
    (state+ o :cant-aim nil)))

(defn revenge!
  [obj]
  (state+ obj :revenge 3000)
  (state+ obj :stagger 0)
  (state+ obj :aims-at nil)
  (inc-cant-aim obj)
  (hook+ obj :update :revenge-timer
         (fn [o _ {:keys [dt]}]
           (let [r (update-state! o :revenge - dt)]
             (when (<= r 0)
               (state+ obj :revenge nil)
               (dec-cant-aim obj)
               
               (let [revenge-damage (state obj :revenge-damage)
                     o (explode! (.-position obj)
                                 {:model blue-explosion
                                  :range (* revenge-damage
                                            2)
                                  :height (* revenge-damage
                                             2.5)
                                  :damage (state obj :revenge-damage)})]
                 ;; shouldn't hurt itself
                 (state+ o :damaged #{obj}))
               
               (state+ obj :revenge-damage 0)
               
               (hooks/hook- obj :update :revenge-timer))))))

(defn damage
  [obj amount dealer]
  (if (state obj :revenge)
    (do (println "revenge!"
                 (update-state! obj :revenge-damage + (* 0.5 amount)))
        (update-state! obj :hp - (* 0.5 amount))
        (update-state! obj :stagger #(+ (or % 0) (* 1000 amount 0.1)))
        (tw/shake-pos! (.-position obj) (THREE/Vector3. 0 0.1 0) #js {:duration 100}))
    
    (do (update-state! obj :hp - amount)
        (update-state! obj :stagger + (* 1000 amount))
        (update-state! obj :hitstun + (* 200 amount))
        (.lookAt obj dealer)
        (anim/play-animation! obj "Got Hit")
        #_(tw/shake-pos! (.-position obj) (THREE/Vector3. 0.1 0 0) #js {:duration 100})))
  
  (when-not (moving? obj)
    (find-target obj))
  
  (save :nts213)
  #_(macroexpand '(update-state! obj :stagger #(+ (or % 0) (* 1000 amount))))
  )

(defn alive?
  [obj]
  (boolean (some-> (state obj :hp)
                   (> 0))))

(defn aims-at?
  [o1 o2]
  (= o2 (state o1 :aims-at)))

(defn attack
  [attacker defender]
  (damage defender (state attacker :damage) attacker))

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
     1
     
     (if (state o :hitstun)
       0
       1)

     #_(if (state o :aims-at) 0.5 1)))

(comment
  #_(.lookAt (first main-chars) (THREE/Vector3. -30 0 1))
  
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
  (state obj)
  (let [hp-bar (state obj :hp-bar)
        max-hp-bar (state obj :max-hp-bar)
        tp (.. (state hp-bar :target) -position)
        hp (state (state hp-bar :target) :hp)
        max-hp (state hp-bar :target :max-hp)
        percent (max 0 (/ hp max-hp))]
    (save :aoehnts)
    (.. hp-bar -scale (set percent 0.2 0.2))
    (.. hp-bar -position (set (+ (.-x tp) (* 0.5 (- (- 1 percent)))) (+ (.-y tp) 1.8) (.-z tp)))
    
    (.. max-hp-bar -scale (set 1 0.2 0.2))
    (.. max-hp-bar -position (set (.-x tp) (+ (.-y tp) 1.7999) (.-z tp)))))

(defn give-hp-bar
  [target]
  (save :thnoseahs)
  (if-not (state target :hp-bar)
    (p/let [gs (assets/load-texture "concrete_powder_green.png")
            rs (assets/load-texture "concrete_powder_red.png")]
      (let [hp (sprite! rs)
            max-hp (sprite! #js {:color 0x111111})]
        
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
                 (.. obj -position (set (+ (or (state button :offset) 0) x)
                                        10
                                        (- (+ z (or (state button :offset-y) 0)) 10))))))
      
      button)))  


(defn create-line!
  ([start end]
   (create-line! start end #js {:color 0x0000ff}))
  ([start end opts]
   (set! (.. start -y) (+ 0.1 (.. start -y)))  
   (set! (.. end -y) (+ 0.1 (.. end -y)))
   
   (let [line-mat (THREE/LineBasicMaterial. opts)
         points #js [start end]
         line-geom (-> (THREE/BufferGeometry.) (.setFromPoints points))
         line (THREE/Line. line-geom line-mat)]
     (ensure-state! line)
     (.add ui line) 

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
  (cond (state o :revenge) "Revenge"
        (state o :dragged) "Dragged"
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
    (set! (.. raycaster -far) (+ (state shooter :range) adjust-distance))
    (.set raycaster from-pos target-dir)                    
    (->> (.intersectObjects raycaster (into-array
                                       (filter #(and (not= ui %)
                                                     (not= shooter
                                                           (or (state % :parent)
                                                               %)))
                                               (.-children gayo-data/scene)))
                            true)
         (filter #(some? (.-face %)))
         (map #(do (when-let [p (state (.-object %) :parent)]
                     (set! (.-object %) p))
                   %)))))

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
            (if (< n hc)
              (attack shooter target)
              (when-let [s (and (= (state target :aims-at) shooter)
                                (state target :shield))]
                (tw/shake-pos! (.-position s) (THREE/Vector3. 0.05 0.05 0) #js {:duration 100}))))
          (do (println shooter "lost target..." (state shooter :aims-at))
              (state+ shooter :aims-at nil)))))))

(defn try-shoot
  [obj]
  
  (doseq [enemy (sort-by #(.distanceTo (.-position obj)
                                       (.-position %))
                         (filter alive? (enemies-of obj)))
          :when (not (state obj :cooldown))]
    (let [dir (THREE/Vector3.)
          target-pos (.clone (.-position enemy))
          from-pos (.clone (.-position obj))
          _ (set! (.. target-pos -y) (+ 0.4 (.. target-pos -y)))
          _ (set! (.. from-pos -y) (+ 0.4 (.. from-pos -y)))
          target-dir (.normalize (.subVectors dir target-pos from-pos))]
      (when-let [target (some-> (first (aim obj from-pos target-dir)) .-object)]
        #_(create-line! from-pos target-pos)
        (if (some->> (state obj :aims-at) (alive?))
          (try
            (let [distance (.distanceTo from-pos (.. target -position))]
              (if (<= distance (state obj :range))
                (do (state+ obj :cooldown (+ cooldown (rand-int 100)))
                    (js/setTimeout #(shoot obj) reaction-time))
                (do (state+ obj :cooldown 1)
                    (let [dir (-> (.subVectors
                                   (THREE/Vector3.)
                                   (.-position target)
                                   (.-position obj))
                                  .normalize
                                  (.multiplyScalar adjust-distance))]
                      (state+ obj :target (.. obj -position (clone) (add dir)))))))
            (catch js/Error e
              (save :wtf2)
              (throw e))
            
            )
          (when (some->> target (enemy-of? obj))
            (save :crgeroca)
            (state+ obj :cooldown (+ cooldown (rand-int 100)))
            
            (state+ obj :aims-at target)
            
            (js/setTimeout #(shoot obj) reaction-time))))))
  
  (when-not (state obj :cooldown)
    (when (and (state obj :aims-at)
               (not (alive? (state obj :aims-at))))
      (state+ obj :aims-at nil))
    (state+ obj :cooldown short-cooldown)))

(defn take-aim
  [obj]
  (if (state obj :cant-aim)
    (state+ obj :aims-at nil)
    (when (and (not (moving? obj))
               (not (state obj :cooldown)))
      (try-shoot obj))))

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
                    {:pos (.-position enemy)
                     :distance (.distanceTo target-pos (.-position obj))}))))
          
          potential-targets (filter some? potential-targets)]
      (if-let [{:keys [pos distance]} (first (sort-by :distance potential-targets))]
        (do
          (if (> distance (- (state obj :range) 0.5))
            (let [dir (-> (.subVectors
                           (THREE/Vector3.)
                           pos
                           (.-position obj))
                          .normalize
                          (.multiplyScalar adjust-distance))]
              (state+ obj :target (.. obj -position (clone) (add dir))))
            #_(state+ obj :target pos)
            (state+ obj :target (.. obj -position))))))))

(defn find-target-in-ball
  [obj]
  (when-not (or (some->> (state obj :aims-at) (alive?))
                (state obj :dragging))
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

(defn reduce-hitstun
  [obj _ {:keys [dt] :as data}]
  (when-let [cd (state obj :hitstun)]
    (state+ obj :hitstun (- cd dt))
    (when (<= (state obj :hitstun) 0)
      (state+ obj :hitstun nil))))

(defn move-to-target
  [obj]
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
          (scene/remove-obj! l))      
        
        (state+ target :line
                (create-line! (.. obj -position (clone))
                              (.. target -position (clone))))))))

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

(def far-range 3)
(def close-range 1)
(def skill-buttons [nil nil nil])

(defn refresh-target-ball
  [obj]
  (let [tb (state obj :target-ball)]
    (.. tb -scale (set 1 1 1))
    (set! (.. tb -material -opacity)
          (if (state obj :dragging)
            0.5
            (if (= selected obj)
              0.3
              0.0)))
    
    (when-let [t (state obj :target)]
      (.. tb -position (copy t)))
    (set! (.. tb -material -map)
          (.. (state obj :model) -material -map))))

(defn refresh-range-ball
  [obj]
  (let [tb (state obj :range-ball)
        range (* 3 (state obj :range))]
    
    (if (state obj :revenge)
      (.. tb -scale (set (* 2 (state obj :revenge-damage))
                         1
                         (* 2 (state obj :revenge-damage))))
      (.. tb -scale (set range 1 range)))
    
    (set! (.. tb -material -opacity)
          (if (state obj :dragging)
            0.5
            (if (= selected obj)
              0.3
              0.0)))
    
    (.. tb -position (copy (.. obj -position)))
    
    (set! (.. tb -material -map)
          (.. (state obj :model) -material -map))))

(defn drag-target
  [obj pos]
  (.. (state obj :target-ball) -position (copy (.-position obj))) 
  (select-character obj) 
  (state+ obj :aims-at nil)
  
  (do (.setFromCamera raycaster pos (.. gayo-data/view -camera))
      (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
        (set! hitted intersects)
        (when-not (seq (clickables intersects))
          (when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
            (let [tb (state obj :target-ball)
                  ;;target (state+ c :target (.clone (.-point hit-floor)))
                  ]
              
              (.. tb -position (copy (.-point hit-floor)))
              
              (when-let [l (state tb :line)]
                (scene/remove-obj! l))      
              
              (state+ tb :line
                      (create-line! (.. obj -position (clone))
                                    (.. tb -position (clone))))))))))

(defn set-target
  [o pos]
  (when-let [p (point-on-floor pos (.. gayo-data/view -camera))]
    (state+ o :target p)))

(defn add-target-ball!
  [c]
  (when-not (state c :target-ball)
    (state+ c :target-ball (.clone target-ball))
    (set! (.. (state c :target-ball) -material)
          (.clone (.-material target-ball)))
    
    (.add ui (state c :target-ball)))
  
  (ensure-state! (state c :target-ball))
  
  (set! (.. (state c :target-ball) -material -blending) THREE/AdditiveBlending)    
  (set! (.. (state c :target-ball) -material -depthWrite) false)    
  (set! (.. (state c :target-ball) -material -depthTest) true)
  (set! (.. (state c :target-ball) -material -transparent) true)
  
  (set! (.. (state c :target-ball) -material -opacity)
        0.5))

(defn clone!
  [o]
  (let [n (.clone o)]
    (ensure-state! n)
    n))

(defn add-shield
  [o]
  (let [s (or (state o :shield) (clone! shield))]
    (save :Tnsaohe)
    (state+ o :shield s)
    (.add o s)
    (.. s -position (set 0 0.4 0.3))
    (state+ s :parent o)))

(defn fix-look
  [o]
  (when-let [t (or (some-> (state o :aims-at) .-position)
                   (state o :target))]
    (.lookAt o t)))

(defn reset-chars!
  []
  (set! main-chars [(find-named "Pink")
                    (find-named "Yellow")
                    (find-named "Blue")])
  
  (set! enemies (find-mesh gayo-data/scene #(some-> (.. % -userData -name) (str/starts-with? "Enemy"))))
  
  (set! floor (find-named "Plane"))
  (set! target-ball (find-named "Target"))
  (set! throwable-projectile (find-named "Throwable Projectile"))
  
  (set! shield (find-named "Shield"))
  
  (set! explosion (find-named "Explosion"))
  (set! (.. explosion -material -transparent) true)
  (set! (.. explosion -material -opacity) 0.4)
  
  (set! blue-explosion (find-named "Blue Explosion"))
  (set! (.. blue-explosion -material -transparent) true)
  (set! (.. blue-explosion -material -opacity) 0.4)
  
  (doseq [c main-chars]
    (state+ c :model (or (find-mesh-name-starts-with c "Animation Root")
                         c))    
    
    (add-shield c)
    
    (ensure-state! (state c :model))
    (state+ (state c :model) :parent c)
    
    (state+ c :cant-aim nil)
    (state+ c :drag #'drag-target)    
    (state+ c :drag-release #'set-target)    
    
    (add-target-ball! c)
    
    (when-not (state c :range-ball)
      (state+ c :range-ball (.clone target-ball))
      (set! (.. (state c :range-ball) -material)
            (.clone (.-material target-ball)))
      
      (.add ui (state c :range-ball)))
    
    (let [hb (find-mesh-name-starts-with c "Hitbox")]
      (set! (.-visible hb) false)
      (ensure-state! hb)
      (state+ hb :parent c)
      (state+ hb :hitbox true))
    
    (ensure-state! (state c :range-ball))        
    
    (set! (.. (state c :range-ball) -material -blending) THREE/AdditiveBlending)    
    (set! (.. (state c :range-ball) -material -depthWrite) false)    
    (set! (.. (state c :range-ball) -material -depthTest) true)
    (set! (.. (state c :range-ball) -material -transparent) true)
    
    (set! (.-visible c) true)
    
    (state+ c :speed 0)
    (state+ c :max-hp 30)
    (state+ c :hp (state c :max-hp))
    
    (state+ c :damage 1)
    
    (give-hp-bar c)
    
    (hook+ c :update        :move-to-target #'move-to-target)
    (hook+ c :second-update :look-at-target-or-aim #'fix-look)
    (hook+ c :second-update :show-status #'show-status)
    (hook+ c :second-update :refresh-target-ball #'refresh-target-ball)
    (hook+ c :second-update :refresh-range-ball  #'refresh-range-ball)
    (hook+ c :second-update :show-hit-chance #'show-hit-chance)
    (hook+ c :update :die #'die)
    (hook+ c :update :take-aim #'take-aim)
    ;;(hook+ c :update :find-target #'find-target-in-ball)
    (hook+ c :update :reduce-cd #'reduce-cd)
    (hook+ c :update :reduce-stagger #'reduce-stagger)
    (hook+ c :update :reduce-hitstun #'reduce-hitstun))
  
  (state+ (first main-chars) :range close-range)
  (state+ (first main-chars) :hit-chance-bonus 2)
  (state+ (first main-chars) :damage 3)
  (state+ (second main-chars) :range far-range)
  (state+ (get main-chars 2) :range far-range)
  
  (set! selected (first main-chars))
  
  (doseq [enemy enemies]
    (ensure-state! enemy)
    (state+ enemy :cant-aim nil)
    
    (add-shield enemy)
    
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
    (hook+ enemy :update :reduce-stagger #'reduce-stagger)
    (hook+ enemy :update :reduce-hitstun #'reduce-hitstun)))

(comment
  (.-position (second (.-children (first main-chars))))
  )

(when ms/debugging
  (reset-chars!))



(defn drag-skill
  [obj pos]
  (when-let [p (point-on-floor pos)]
    (let [tb (state obj :target-ball)]
      
      (.. tb -position (copy p))
      
      (when-let [l (state tb :line)]
        (scene/remove-obj! l))      
      
      (state+ tb :line
              (create-line! (.. selected -position (clone))
                            (.. tb -position (clone))))))
  (println "skill!"))

(defn release-skill
  [button]
  (println "skill!")
  
  (let [o (.clone throwable-projectile)]
    (ensure-state! o)
    (.. gayo-data/scene (add o))
    (let [tb (state button :target-ball)]
      (.. o -position (copy (.-position selected)))
      (js/setTimeout (fn []
                       (explode! (.-position o) {:height 5, :range 5})
                       (scene/remove-obj! o)) 1000)
      (tw/tween-pos! (.-position o) (.clone (.-position tb))))))

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
    (ensure-state! light)
    (save :add-light1)
    (set! (.. light -intensity) 2)
    (.add scene light))
  
  (reset-chars!)
  
  (doseq [i (range (count main-chars))
          :let [c (get main-chars i)
                offset (- (* i 1.5) 1.5)]]
    (p/then (add-button! #'select)
            (fn [b]
              (println "adding button")
              (set! (.. b -material -map) (.. (state c :model) -material -map))
              (state+ b :character c)
              (state+ c :button b)
              (state+ b :offset offset)
              (select b))))
  
  (p/then (add-button! nil)
          (fn [b]
            (set! (.-name b) "Left Skill")
            (set! (.. b -material -map) (.. (state (get main-chars 0) :model) -material -map))
            (state+ b :offset -1.5)
            (state+ b :offset-y 1.5)
            
            (state+ b :drag #'drag-skill)
            (state+ b :drag-release #'release-skill)
            
            (set! skill-buttons (assoc skill-buttons 0 b))
            
            (add-target-ball! b)))
  
  (p/then (add-button! #(revenge! selected))
          (fn [b]
            (set! (.-name b) "Middle Skill")
            (set! (.. b -material -map) (.. (state (get main-chars 1) :model) -material -map))
            (state+ b :offset 0)
            (state+ b :offset-y 1.5)
            
            (set! skill-buttons (assoc skill-buttons 1 b))
            
            (add-target-ball! b)))
  
  (p/then (add-button! nil)
          (fn [b]
            (set! (.-name b) "Right Skill")
            (set! (.. b -material -map) (.. (state (get main-chars 2) :model) -material -map))
            (state+ b :offset 1.5)
            (state+ b :offset-y 1.5)
            
            (state+ b :drag #'drag-skill)
            (state+ b :drag-release #'release-skill)
            
            (set! skill-buttons (assoc skill-buttons 2 b))
            
            (add-target-ball! b))))





(comment
  (.-children (.-scene gayo-data/loaded-scene))
  
  
  
  (bean (find-named "Light"))
  
  
  )

(def camera-down-pos (THREE/Vector3.))
(def floor-down-pos (THREE/Vector3.))
(def x-scroll-speed 8)
(def y-scroll-speed -5)

(defn draggable?
  [o]
  (state o :drag))

(defn update-drag!
  [pos]
  ((state dragging :drag) dragging pos))

(defn start-drag!
  [o pos]
  (set! dragging o)
  (state+ o :dragged true)
  (update-drag! pos))

(defn update!
  [_ camera _]
  (ms/debug selected {:func #'update!})
  
  (when-not selected
    (select (first (filter alive? main-chars))))
  
  (def camera camera)
  
  (when is-down
    (if dragging
      (update-drag! curr-pos)
      (when-let [pof (point-on-floor curr-pos camera)]
        (let [diff (THREE/Vector2.)
              {:keys [x y z]} (bean camera-down-pos)
              _ (.subVectors diff down-pos curr-pos)
              new-pos1 (.clone (two->three down-pos
                                           camera 
                                           (.distanceTo (.-position camera) pof)))
              new-pos2 (two->three curr-pos
                                   camera 
                                   (.distanceTo (.-position camera)
                                                (point-on-floor curr-pos camera)))]
          
          (.sub new-pos1 new-pos2)        
          
          (.. camera -rotation (set -0.9827 0 0))
          (.. camera -position
              (set
                                        ;(+ x (.-x diff)) y (+ z (.-z diff))
               (+ x (.-x new-pos1))
               20
               (+ z (.-z new-pos1))
               ))
          
          #_    (.. camera (lookAt (.-position selected)))
          ))))
  
  #_(.. camera -rotation)
  
  
  #_(when-let [cam-target selected #_ (first (filter #(> (.. % -state -hp) 0) main-chars))]
      (.. camera -position (set (+ (.. cam-target -position -x) 0)
                                20
                                (+ (.. cam-target -position -z) 20)))      
      
      (.. camera (lookAt (.-position cam-target))))
  
  )

(comment
  hitted
  
  (bean (.-object (first (filter #(not (.-state (.-object %))) hitted))))
  )

(defn end-drag!
  [pos]
  (when-let [o dragging]
    (state+ dragging :dragged false)
    (set! dragging nil)
    ((state o :drag-release) o pos)))

(defn start
  [point scene camera]
  
  (.. camera-down-pos (copy (.-position camera)))
  (set! is-moving false)
  (set! is-moving-far false)
  (set! is-moving-super-far false)
  (set! is-down true)
  (.copy down-pos point)
  (.copy curr-pos point)
  
  (set! down-time gayo-data/last-time)
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster 
                                      (.-children gayo-data/scene)
                                      true)]
    (set! hitted intersects)
    (when-not (seq (clickables hitted))
      (let [main-chars-set (into #{} main-chars)
            _ (save :tnhseoahs)
            hit-draggable
            (->> intersects
                 (filter #(draggable? (or (state (.-object %) :parent)
                                          (.-object %)))))]
        (save :croe12)
        (if-let [to-move (case (count hit-draggable)
                           0 nil
                           1 (first hit-draggable)
                           (->> hit-draggable
                                (sort-by #(if (main-chars-set (.-object %))
                                            -10
                                            (.distanceTo (.-position
                                                          (or (state (.-object %) :parent)
                                                              (.-object %)))
                                                         (.-point %))))
                                first))]
          (do (save :cgreo)
              (let [to-move (or (state (.-object to-move) :parent) (.-object to-move))]
                (save :croe)
                (start-drag! to-move curr-pos)))
          
          ;; hit floor?
          (when-let [floor (first (filter #(= (.-object %) floor) hitted))]
            (.copy floor-down-pos (.-point floor))))
        )))
  
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
  
  (if-not dragging
    (do (.setFromCamera raycaster curr-pos camera)
        (let [intersects (.intersectObjects raycaster (.-children gayo-data/scene) true)]
          (set! hitted intersects)
          
          (if-let [hit-button (first (clickables intersects))]
            ((state (.. hit-button -object) :clicked) (.. hit-button -object) point)
            
            #_(when-let [hit-floor (first (filter #(= (.-object %) floor) hitted))]
                (save :eouantsh)
                (state+ selected :speed 0)
                (state+ selected :target (.clone (.-point hit-floor))))))))
  
  (hooks/run-hooks! :release)
  (hooks/run-hooks! :click)
  
  (end-drag! point)
  
  (.setFromCamera raycaster point camera)
  (let [intersects (.intersectObjects raycaster (.-children scene))]
    
    )
  
  (set! is-moving false)
  (set! is-down false)) 
