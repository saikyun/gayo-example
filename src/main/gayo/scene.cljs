(ns gayo.scene
  (:require [clojure.string :as str]
            [gayo.state :as state]
            [gayo.hooks :as hooks]
            [gayo.log :refer [log!]]
            [miracle.save :refer-macros [save save-do]]))

(defn remove-obj!
  [o]
  (when o
    (save :hntsaoehns)
    (hooks/hook- o)
    (if (.-parent o)
      (.remove (.-parent o) o)
      (log! o "has no parent!"))))

(defn clear-obj!
  [obj]
  (let [to-remove #js []]
    (doseq [c (.-children obj)]
      (.push to-remove c))
    (doseq [o to-remove]
      (.remove obj o)))
  (set! (.. obj -children -count) 0))


(defn find-mesh
  [scene pred]
  (let [vs (transient [])]
    (.traverse scene #(when (pred %) (conj! vs %)))
    (persistent! vs)))

(defn find-mesh-by-name
  [scene name]
  (first (find-mesh scene #(= (.. % -userData -name) name))))

(defn find-mesh-name-starts-with
  [scene name]
  (first (find-mesh scene #(str/starts-with? (.. % -userData -name) name))))

(defn find-meshes-by-data
  [scene pred]
  (find-mesh scene #(pred (.. % -userData))))

(defn go->top-go
  [go]
  (or (some-> go .-state .-topGo)
      go))

(defn go->card
  [go]
  (some-> (go->top-go go) .-cardId))

(defn go->zone-go
  [go]
  (let [top-go (go->top-go go)]
    (or (and
         (some-> top-go .-state .-zoneId)
         top-go)
        (and
         (some-> top-go .-parent .-state .-zoneId)
         (.-parent top-go)))))

(defn handle-attributes!
  [obj]
  (state/ensure-state! obj)
  
  (when (.. obj -userData -transparent)
    (log! "TRANSPARENT")
    (set! (.. obj -material -transparent) true)
    (set! (.. obj -material -needsUpdate) true))
  
  #_  (when (.-isMesh obj)
        (set! (.-receiveShadow obj) true))      
  
  (when (or (-> obj .-userData (aget "deckHitbox"))
            (-> obj .-userData (aget "libraryHitbox")))
    (log! "DECK OR LIB")
    (save :QUE_PASTA)
    (set! (.. obj -material -visible) false))
  
  (when (.. obj -userData -tile)
    (set! (.. obj -userData -landable) true))
  
  (state/ensure-state! obj)
  (set! (.. obj -state -orgPos) (.. obj -position (clone)))
  (set! (.. obj -state -orgScale) (.. obj -scale (clone)))
  
  (when (.. obj -userData -foldable)
    (set! (.. obj -originalPosition) (.. obj -position (clone)))
    (.. obj -geometry computeBoundingBox)))

(comment
  (->> (let [stuff (transient [])]
         (.traverse gayo.data/scene
                    (fn [v] (when-not (.-state v)
                              (conj! stuff v))))
         (persistent! stuff))
       (map cljs-bean.core/bean))
  
  (persistent! (conj!  (transient []) :a))
  )
