(ns gayo.hooks
  (:require [miracle.save]
            [gayo.log :refer [log!]]
            [gayo.state :refer [ensure-state!]]
            [gayo.scene :refer [go->top-go]])
  (:require-macros [miracle.save :refer [save save-do]]))

(defonce all-hooks (atom {}))

(defprotocol Runnable (run [this] [this data]))

(defrecord hook
    [obj kind hook-name f opts]
  Runnable
  (run [this]
    (f obj kind))
  (run [this data]
    (f obj kind data)))

(def global-hooks-obj #js [])

(defn hook+
  ([obj kind hook-name f]
   (hook+ obj kind hook-name f nil))
  ([obj kind hook-name f opts]
   (swap! all-hooks update kind
          (fn [hs] 
            (conj
             (filter #(not (and (= (:obj %) obj)
                                (= (:hook-name %) hook-name))) hs)
             (->hook obj kind hook-name f opts))))
   :ok))

(defn clear-all-hooks!
  []
  (reset! all-hooks {}))

(defn hook-
  ([{:keys [obj kind hook-name]}]
   (hook- obj kind hook-name)
   :ok)
  ([robj rkind]
   (swap! all-hooks update rkind
          #(filter (fn [{:keys [obj kind hook-name]}]
                     (not (and (= obj robj)
                               (= kind rkind))))
                   %))
   :ok)
  ([robj rkind rhook-name]  
   (swap! all-hooks update rkind
          #(filter (fn [{:keys [obj kind hook-name]}]
                     (not (and (= obj robj)
                               (= kind rkind)
                               (= hook-name rhook-name))))
                   %))
   :ok))

(defn ghook+
  ([kind hook-name f]
   (hook+ global-hooks-obj kind hook-name f nil))
  ([kind hook-name f opts]
   (hook+ global-hooks-obj kind hook-name f opts)))

(defn ghook-
  ([kind]
   (hook- global-hooks-obj kind))
  ([kind hook-name]
   (hook- global-hooks-obj kind hook-name)))

(defn run-hooks!
  ([kind]
   (run-hooks! kind nil))
  ([kind data]
   (doseq [h (get @all-hooks kind)]
     (try
       (if data
         (run h)
         (run h data))
       (catch js/Error e
         (save :hook-error)
         (hook- h)
         (throw e)))
     (when (some-> h :opts :once)
       (hook- h)))))

(defn run-hooks-js!
  [kind]
  (doseq [h (-> @all-hooks (get kind))]
    ((.-func h) (.-obj h) (.-hookName h))))

(comment
  (def o1 #js {})
  
  (hook+ o1 :update :yeah log!)
  (hook+ o1 :update :yeah2 #(log! "2" %&))
  
  (hook- o1 :update :yeah)
  (hook- o1 :update)
  
  (clear-all-hooks!)
  )

(defn make-clickable!
  [o f]
  (ensure-state! o)
  (set! (.. o -state -onClick) f))

(defn clickable?
  [o]
  (some-> (go->top-go o) .-state .-onClick))

(defn make-holdable!
  [o down-f up-f]
  (ensure-state! o)
  (set! (.. o -state -hold) #js {:down down-f
                                 :up up-f}))

(defn holdable?
  [o]
  (some-> (go->top-go o) .-state .-hold))

(defn draggable?
  [o]
  (some-> (go->top-go o) .-state .-draggable))
