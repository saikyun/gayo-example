(ns gayo.tweens
  (:require [cljs-bean.core :refer [bean ->js]]
            [gayo.log :refer [log!]]
            ["three" :as THREE]
            [gayo.state :refer [ensure-state!]]
            )
  (:require-macros [miracle.save :refer [save]]))

(defonce tweens #js [])
(defonce tweens-to-add #js [])
(defonce queued-tweens #js [#js []
                            #js []
                            #js []])
(defonce last-dt nil)

(defn linear
  [percent]
  percent)

(defn ease-in-cubic
  [percent]
  (.pow js/Math percent 3))

(defn ease-out-cubic
  [percent]
  (- 1 (.pow js/Math (- 1 percent) 3)))

(defn ease-out-elastic
  [percent]
  (let [p 0.5]
    (+ 1
       (* (.pow js/Math 2 (* percent -10))
          (.sin js/Math (/ (* (- percent (/ p 4))
                              (* 2 (.-PI js/Math)))
                           p))))))

(defn shake-target
  [percent]
  (let [percent2 (* 0.01 (rem (int (* 100 percent)) 25))
        multiplier (condp < percent
                     0.99   0
                     0.75 -1
                     0.5   1
                     0.25  -1
                     1)
        p (* 4 percent2)]
    (* multiplier (ease-out-cubic p))))

(defn up-down
  [percent]
  (let [#_#_percent2 (* 0.01 (rem (int (* 100 percent)) 50))
        multiplier (condp < percent
                     0.99  0.1
                     0.5   1
                     0)
        #_ #_ p (* 2 percent2)]
    (* multiplier (ease-out-cubic percent))))

(defn bounce
  [percent]
  (let [p 1.5]
    (+ 1
       (* (.pow js/Math 2 (* percent -10))
          (.sin js/Math (/ (* (- percent (/ p 4))
                              (* 2 (.-PI js/Math)))
                           p))))))

(defn ease-mult
  [start-v target-v easing percent]
  (let [res (+ start-v 
               (* (- target-v start-v) (easing percent)))]
    res))

(defn ease-add
  [start-v target-v easing percent]
  (let [res (+ start-v (* target-v (easing percent)))]
    res))

(defn update-pos
  [o start target easing percent ease]
  (.set o
        (ease (.-x start) (.-x target) easing percent)
        (ease (.-y start) (.-y target) easing percent)
        (ease (.-z start) (.-z target) easing percent)))

(defn update-color!
  [color _ target easing percent ease]
  (.lerp color target percent))

(defn update-opacity!
  [o start target easing percent ease]
  (save :que-pasta)
  (set! (.. o -opacity) (ease start target easing percent))
  o)

(defn add-tween!
  [o]
  (when (and (.-o o)
             (.-target o)
             (not (.-start o)))
    (set! (.-start o) (.clone (.-o o))))
  
  (when-not (.-time o)
    (set! (.-time o) 0))
  
  (when-not (.-easing o)
    (set! (.-easing o) linear))
  
  (set! (.-time o) (+ last-dt (.-time o)))
  (.push tweens-to-add o)
  o)

(defn add-queued-tween!
  [o]
  (when (and (.-o o)
             (.-target o)
             (not (.-start o)))
    (set! (.-start o) (.clone (.-o o))))
  
  (when-not (.-easing o)
    (set! (.-easing o) linear))  

  (when-not (.-time o)
    (set! (.-time o) 0))
  (set! (.-time o) (+ last-dt (.-time o)))
  
  (let [level (or (.-level o) 1)]
    (.push (nth queued-tweens level) o))
  
  o)

(defn reset-tween!
  [o target]
  (set! (.-start o) (.clone (.-o o)))
  (set! (.-time o) last-dt)
  (set! (.-target o) target)
  o)

(defn finished-tween?
  [t]
  (or (not t)
      (.-finished t)
      (>= (.-time t)
          (.-duration t))))

(defn remove-tween!
  ([t]
   (remove-tween! t true))
  ([t run-end]
   (when t
     (set! (.-finished t) true)
     (when-not run-end
       (set! (.-end t) nil)))))

(defn tween-pos*
  [pos target opts]
  (let [tween (.assign js/Object
                       #js {:o pos
                            :duration 500
                            :f update-pos
                            :easing ease-out-cubic
                            :target target}
                       opts)
        holder (if-let [p (.-parent opts)]
                 p
                 pos)]
    (remove-tween! (.. holder -state -tweenPos))
    tween))

(defn tween-pos!
  ([pos target]
   (tween-pos! pos target #js {}))
  ([pos target opts]
   (let [holder (if-let [p (.-parent opts)] p pos)]
     (ensure-state! holder)
     (set! (.. holder -state -tweenPos)
           (if (.-queued opts)
             (add-queued-tween! (tween-pos* pos target opts))
             (add-tween! (tween-pos* pos target opts)))))))

(defn tween-pos-q!
  ([pos target]
   (tween-pos-q! pos target #js {}))
  ([pos target opts]
   (let [holder (if-let [p (.-parent opts)] p pos)]
     (ensure-state! holder)
     (set! (.. holder -state -tweenPos)
           (add-queued-tween! (tween-pos* pos target opts))))))

(defn shake-pos!
  ([pos target]
   (shake-pos! pos target #js {}))
  ([pos target opts]
   (let [tween
         (.assign js/Object
                  #js {:o pos
                       :duration 500
                       :f update-pos
                       :easing shake-target
                       :ease ease-add
                       :target target}
                  opts)
         holder (if-let [p (.-parent opts)] p pos)]
     (ensure-state! holder)
     (save :yee-boi-shake-pos123)
     (remove-tween! (.. holder -state -shakePos))
     (set! (.. holder -state -shakePos) (add-tween! tween)))))

(defn delay!
  ([opts]
   (let [tween opts]
     (add-tween! tween)))
  ([delay f]
   (delay! delay f #js {}))
  ([delay f opts]
   (delay! (.assign js/Object #js {:duration delay
                                   :end f}
                    opts))))

(defn queued-delay!
  ([opts]
   (let [tween opts]
     (add-queued-tween! tween)))
  ([delay f]
   (queued-delay! delay f #js {}))
  ([delay f opts]
   (queued-delay! (.assign js/Object #js {:duration delay
                                          :end f}
                           opts))))

(comment
  (do
    (delay! 2000 #(log! "yeah my boi"))
    (delay! 1000 #(log! "yeah my boi")))
  
  (do
    (queued-delay! 1000 #(log! "yeah my boi"))
    (queued-delay! 1000 #(log! "yeah my boi")))
  )

(defn effect!
  [opts]
  (let [tween opts]
    (add-tween! tween)))

(defn tween-scale!
  ([scale target]
   (tween-scale! scale target #js {}))
  ([scale target opts]
   (let [holder (if-let [p (.-parent opts)] p scale)
         _ (ensure-state! holder)
         existing-tween (.. holder -state -tweenScale)]
     (save :mah-boi)
     (cond
       (and existing-tween
            (.-kind existing-tween)
            (.-kind opts)
            (= (.-kind existing-tween) 
               (.-kind opts)))
       :ok
       
       :else
       (let [tween (.assign js/Object
                            #js {:o scale
                                 :duration 500
                                 :f update-pos
                                 :easing ease-out-cubic
                                 :target target}
                            opts)]
         (remove-tween! existing-tween)
         (set! (.. holder -state -tweenScale) (add-tween! tween)))))))

(defn tween-rotation!
  ([rotation target]
   (tween-rotation! rotation target #js {}))
  ([rotation target opts]
   (let [holder (if-let [p (.-parent opts)] p rotation)
         _ (ensure-state! holder)
         existing-tween (.. holder -state -tweenRotation)]
     (save :mah-boi)
     (cond
       (and existing-tween
            (.-kind existing-tween)
            (.-kind opts)
            (= (.-kind existing-tween) 
               (.-kind opts)))
       :ok
       
       :else
       (let [tween (.assign js/Object
                            #js {:o rotation
                                 :duration 500
                                 :f update-pos
                                 :easing ease-out-cubic
                                 :target target}
                            opts)]
         (remove-tween! existing-tween)
         (set! (.. holder -state -tweenRotation) (add-tween! tween)))))))

(defn tween-color!
  ([color target]
   (tween-color! color target #js {}))
  ([color target opts]
   (let [tween (.assign js/Object
                        #js {:o color
                             :duration 500
                             :f update-color!
                             :easing ease-out-cubic
                             :target target}
                        opts)
         holder (if-let [p (.-parent opts)] p color)]
     (ensure-state! holder)
     (remove-tween! (.. holder -state -tweenColor))
     (set! (.. holder -state -tweenColor) (add-tween! tween)))))

(defn tween-mat-opacity!
  ([mat target]
   (tween-mat-opacity! mat target #js {}))
  ([mat target opts]
   (ensure-state! mat)
   (set! (.. mat -transparent) true)
   (let [opacity-tween (.assign js/Object
                                #js {:duration 400
                                     :f update-opacity!
                                     :o mat
                                     :easing (if (> 0 target)
                                               ease-in-cubic
                                               ease-out-cubic)
                                     :target target
                                     :start (.. mat -opacity)}
                                opts)]
     (remove-tween! (.. mat -state -opacityTween))
     (set! (.. mat -state -opacityTween)
           (add-tween! opacity-tween)))))


(defn update-volume!
  [o start target easing percent ease]
  (.setVolumeAsync o (ease start target easing percent))
  o)

(defn tween-volume!
  ([sound target]
   (tween-volume! sound target #js {}))
  ([sound target opts]
   (ensure-state! sound)
   (set! (.. sound -transparent) true)
   (let [volume-tween (.assign js/Object
                               #js {:duration 400
                                    :f update-volume!
                                    :o sound
                                    :easing (if (> 0 target)
                                              ease-out-cubic
                                              ease-in-cubic)
                                    :target target}
                               opts)]
     (remove-tween! (.. sound -state -volumeTween))
     (set! (.. sound -state -volumeTween) (add-tween! volume-tween)))))

(defn tween-opacity!
  ([obj-or-mat target]
   (tween-opacity! obj-or-mat target #js {}))
  ([obj-or-mat target opts]
   
   (ensure-state! obj-or-mat)
   (when-let [f (.-end opts)]
     (remove-tween! (.. obj-or-mat -state -opacityDelayTween))
     (set! (.. obj-or-mat -state -opacityDelayTween)
           (delay! (or (.-duration opts) 400) f)))
   
   (js-delete opts "end")
   
   (if (not (.-traverse obj-or-mat))
     (tween-mat-opacity! obj-or-mat target opts)
     (let [mat (.. obj-or-mat -material)]
       (when mat
         (tween-mat-opacity! mat target opts))
       (.traverse obj-or-mat #(when-let [mat (.. % -material)]
                                (tween-mat-opacity! mat target  opts)))))))

(defn queued-tweens?
  []
  (or (first (nth queued-tweens 0))
      (first (nth queued-tweens 1))
      (first (nth queued-tweens 2))))

(defn run-tween!
  [t dt]
  (set! (.-time t) (+ dt (.-time t)))
  (let [{:keys [finished end f start easing o duration target ease] :or {ease ease-mult}} (bean t)]
    (when (and (not finished) f)
      (try
        (f (->js o) start target easing
           (max 0 (min 1 (/ (.-time t) duration)))
           ease)
        (catch js/Error e
          (save :anim-error)
          (.error js/console e)
          (remove-tween! t))))
    
    (when (finished-tween? t)
      (when (not finished)
        (when f
          (try
            (f (->js o) start target easing 1 ease)
            (catch js/Error e
              (save :anim-error-finished)
              (.error js/console e))))
        (when end
          (try
            (end)
            (catch js/Error e
              (save :anim-error-end)
              (.error js/console e)))))))
  
  t)

(defn update-queued-tweens!
  [dt]
  (when (> dt 0)
    (let [q (or (and (first (nth queued-tweens 0)) (nth queued-tweens 0)) 
                (and (first (nth queued-tweens 1)) (nth queued-tweens 1)) 
                (and (first (nth queued-tweens 2)) (nth queued-tweens 2)))]
      (when-let [t (first q)]
        (let [t (run-tween! t dt)]
          (when (finished-tween? t)
            (.splice q 0 1)
            (recur (- dt (if (> 0 (- (.-duration t) (.-time t)))
                           0
                           (- (- (.-duration t) (.-time t))))))))))))

(defn update-tweens!
  [dt]
  (set! last-dt dt)
  (update-queued-tweens! dt)
  
  (doseq [t tweens-to-add]
    (.push tweens t))
  (set! (.-length tweens-to-add) 0)
  
  (loop [i 0]
    (if (>= i (.-length tweens))
      tweens
      (let [t (run-tween! (nth tweens i) dt)]
        (if (finished-tween? t)
          (do (.splice tweens i 1)
              (recur i))
          (recur (inc i)))))))

(defn clear-tweens!
  []
  (set! tweens #js [])
  (set! queued-tweens #js [#js []
                           #js []
                           #js []]))

(comment
  (test.tweens/clear-tweens!) ; reset
  
  )
