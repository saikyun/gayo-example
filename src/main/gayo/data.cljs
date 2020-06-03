(ns gayo.data
  (:require ["three" :as THREE]
            [gayo.storage :as st]
            
            [gayo.log :refer [log!]]
            [reagent.core :as r]
            [gayo.scene :refer [find-mesh
                                find-mesh-by-name
                                find-meshes-by-data]]            
            
            
            [cljs.reader :as read]            
            [promesa.core :as p]
            [miracle.save]
            [cljs-bean.core :refer [bean ->clj ->js]]
            [miracle.save :refer-macros [save save-do]]))

(defonce dont-render false)

(defonce glc nil)
(defonce scene nil)

(comment
  (.traverse scene println)
  
  (.-camera view)
  )

(defonce renderer nil)
(defonce composer nil)
(defonce obj nil)
(defonce clj-meshes {})
(defonce meshes (js-obj))
(defonce def-hexa nil)
(defonce reactive #js {})
(defonce npc #js {})
(defonce input-queue #js [])
(defonce loaded-scene nil)
(defonce loading false)
(defonce max-ani nil)
(defonce clips nil)
(defonce mixers #js [])
(defonce views #js [])
(defonce dims (THREE/Vector4.))
(defonce fps-text nil)
(defonce view nil)
(def last-time nil)

(defonce rg! nil)

(defonce db-watch nil)
(defonce cleanup-scene! nil)

(defonce update! nil)
(defonce move nil)
(defonce start nil)
(defonce release nil)

(defonce next-msg! nil)

(defonce on-pc?
  (and
   #_false
   (some-> js/window .-document .-hasFocus)))

(defn init-animations
  [animations]
  (set! clips animations))

(defn save-game!
  [game]
  (-> (st/store "savedGame" (pr-str game))
      (p/then #(log! "Saved game!"))))

(defn get-saved-game!
  []
  (-> (st/load "savedGame")
      (p/then #(if (nil? %) "{}" %))
      (p/then read/read-string)))
