(ns gayo.collision
  "Built with the help of: http://www.jeffreythompson.org/collision-detection/"
  (:require ["three" :as THREE]
            [gayo.data :as gayo-data :refer [ui]]
            [gayo.text :refer [set-text!]]
            [goog.string :as gstring]
            [gayo.scene :refer [remove-obj!]]
            [gayo.hooks :as h]
            [gayo.internal])
  (:require-macros [miracle.save :refer [save]]))

(defn two->three
  [v2 y]
  (THREE/Vector3. (.-x v2) y (- (.-y v2))))

(defn create-line!
  ([start end]
   (create-line! start end #js {:color 0x00ff00}))
  ([start end opts]
   (let [start (two->three start 3)
         end   (two->three end 3)
         line-mat (THREE/LineBasicMaterial. opts)
         points #js [start end]
         line-geom (-> (THREE/BufferGeometry.) (.setFromPoints points))
         line (THREE/Line. line-geom line-mat)]
     
     (.add ui line) 
     
     line)))

(defonce line-g nil)
(defonce point-line nil)
(defonce point-lines [])

(when line-g
  (remove-obj! line-g)
  (set! line-g nil))

(def line #js {:start  (THREE/Vector2. (-  (rand 2) 1.5) (-  (rand 2) 1.5))
               :end  (THREE/Vector2. (rand 2) (rand 2))})

(defn gen-poly
  []
  (let [c 1
        c2 (/ c 2)
        tl (THREE/Vector2. (- (rand c2) c) (- (rand c2) c)) 
        tr (THREE/Vector2. (+ (rand c2) c) (- (rand c2) c))
        br (THREE/Vector2. (+ (rand c2) c) (+ (rand c2) c)) 
        bl (THREE/Vector2. (- (rand c2) c) (+ (rand c2) c))]
    [#js {:start tl
          :end  tr}
     #js {:start tr
          :end  br}
     #js {:start br
          :end  bl}
     #js {:start bl
          :end  tl}]))

#_(set! line-g (create-line! (.-start line) (.-end line)))

(def line-len (.distanceTo (.-start line)
                           (.-end line)))

(defn point->line
  [p extra]
  #js {:start (THREE/Vector2. (- (.-x p) extra) (.-y p))
       :end   (THREE/Vector2. (+ (.-x p) extra) (.-y p))})

(defn point-circle
  [p circle-pos radius]
  (if (<= (.distanceTo p circle-pos) radius)
    p
    false))

(defn circle-circle
  [c1-pos r1 c2-pos r2]
  (<= (.distanceTo c1-pos c2-pos) (+ r1 r2)))

(println "Point circle")

(let [p (THREE/Vector2. 0.5 0.5)
      circle #js {:pos (THREE/Vector2. 1 1)
                  :radius 1}]
  (println p
           circle
           (point-circle p (.-pos circle) (.-radius circle))))

(let [p (THREE/Vector2. 1.1 0.5)
      circle #js {:pos (THREE/Vector2. 1 1)
                  :radius 1}]
  (println p
           circle
           (point-circle p (.-pos circle) (.-radius circle))))

(let [p (THREE/Vector2. 2.1 0.5)
      circle #js {:pos (THREE/Vector2. 1 1)
                  :radius 1}]
  (println p
           circle
           (point-circle p (.-pos circle) (.-radius circle))))

(defn line-point
  [lstart lend p]
  (let [line-len (.distanceTo lstart lend)
        start-distance (.distanceTo lstart p)
        end-distance   (.distanceTo lend   p)
        dist (- line-len (+ start-distance end-distance))
        buffer 0.001]
    (and (< dist buffer)
         (> dist (- buffer)))))

#_(h/ghook+ :move 
            :test-coll 
            (fn [_ _ point]
              (when point-line
                (remove-obj! point-line)
                (set! point-line nil))
              
              (let [point (.clone point)
                    _ (.set point (* 3 (.-x point))
                            (* 3 (.-y point)))
                    pl (point->line point 0.01)]
                (set! point-line (create-line! (.-start pl) (.-end pl)
                                               #js {:color (if (line-point (.-start line)
                                                                           (.-end line)
                                                                           point)
                                                             0x00ff00
                                                             0xff00ff)}
                                               )))            
              
              ))




(let [line-circle-point (THREE/Vector2. 0 0)]
  (defn line-circle
    [line-start line-end circle-pos radius]
    (or (point-circle line-start circle-pos radius)
        (point-circle line-end circle-pos radius)
        
        (let [len (.distanceTo line-start line-end)
              _ (save :thnseoausnt)
              dot (/ (+ (* (- (.-x circle-pos)
                              (.-x line-start))
                           (- (.-x line-end)
                              (.-x line-start)))
                        
                        (* (- (.-y circle-pos)
                              (.-y line-start))
                           (- (.-y line-end)
                              (.-y line-start))))
                     (.pow js/Math len 2))
              closest-x (+ (.-x line-start) (* dot (- (.-x line-end) (.-x line-start))))
              closest-y (+ (.-y line-start) (* dot (- (.-y line-end) (.-y line-start))))]
          (.set line-circle-point closest-x closest-y)
          
          ;; visualization start
          #_(set! point-lines
                  (conj point-lines
                        (let [pl (point->line line-circle-point radius)]
                          (create-line! (.-start pl) (.-end pl)
                                        #js {:color 0xff0000}))))
          ;; visualization end
          
          (if (and (line-point line-start line-end line-circle-point)
                   (<= (.distanceTo line-circle-point circle-pos) radius))
            line-circle-point
            false)))))


;; line-circle
#_(h/ghook+ :move 
            :test-coll 
            (fn [_ _ point]
              (doseq [p point-lines]
                (remove-obj! p))
              (set! point-lines [])
              
              (let [radius 0.1
                    point (.clone point)
                    _ (.set point (* 3 (.-x point))
                            (* 3 (.-y point)))
                    pl (point->line point radius)]
                (let [extra [(let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a (.-start line)]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three (.-start line) 3)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a point]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three point 3)))
                               (set! (.. o -position -z)
                                     (+ 0.2 (.. o -position -z)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a (.-end line)]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three (.-end line) 3)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o "0 0")                                   
                               (.. o -position (copy (two->three (THREE/Vector2. 0 0) 3)))
                               o)
                             
                             (create-line! (.-start pl) (.-end pl)
                                           #js {:color (if (line-circle (.-start line)
                                                                        (.-end line)
                                                                        point
                                                                        radius)
                                                         0x00ff00
                                                         0xff00ff)}
                                           )
                             
                             (let [pl (point->line (.-start line) radius)
                                   l (create-line! (.-start pl) (.-end pl)
                                                   #js {:color 0xf0f000})]
                               l)
                             
                             (let [pl (point->line (.-end line) radius)]
                               (create-line! (.-start pl) (.-end pl)
                                             #js {:color 0xf0f000}))]]
                  (set! point-lines (concat point-lines extra))))))


(def poly (gen-poly))

(defn poly-circle
  [poly circle-pos radius]
  (save :tnhsao)
  (reduce (fn [acc p]
            (if-let [coll (line-circle (.-start p) (.-end p) circle-pos radius)]
              (reduced coll)
              acc))
          false
          poly))

;; poly-circle
#_(h/ghook+ :move 
            :test-coll 
            (fn [_ _ point]
              (doseq [p point-lines]
                (remove-obj! p))
              (set! point-lines (vec (map #(create-line! (.-start %) (.-end %)) poly)))
              
              (let [radius 0.1
                    point (.clone point)
                    _ (.set point (* 3 (.-x point))
                            (* 3 (.-y point)))
                    pl (point->line point radius)]
                (let [extra [(let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a (.-start line)]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three (.-start line) 3)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a point]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three point 3)))
                               (set! (.. o -position -z)
                                     (+ 0.2 (.. o -position -z)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o 
                                          (let [a (.-end line)]
                                            (gstring/format "%.2f %.2f" (.-x a) (.-y a))))                                   
                               (.. o -position (copy (two->three (.-end line) 3)))
                               o)
                             
                             (let [o (THREE/Object3D.)]
                               (.add ui o)                                   
                               (set-text! o "0 0")                                   
                               (.. o -position (copy (two->three (THREE/Vector2. 0 0) 3)))
                               o)
                             
                             (create-line! (.-start pl) (.-end pl)
                                           #js {:color (if (poly-circle poly
                                                                        point
                                                                        radius)
                                                         0x00ff00
                                                         0xff00ff)}
                                           )
                             
                             (let [pl (point->line (.-start line) radius)
                                   l (create-line! (.-start pl) (.-end pl)
                                                   #js {:color 0xf0f000})]
                               l)
                             
                             (let [pl (point->line (.-end line) radius)]
                               (create-line! (.-start pl) (.-end pl)
                                             #js {:color 0xf0f000}))]]
                  (set! point-lines (concat point-lines extra))))))
