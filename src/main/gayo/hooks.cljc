(ns gayo.hooks
  (:require [miracle.save]
            [gayo.log :refer [log!]]
            [gayo.state :refer [ensure-state!]])
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

(defonce global-hooks-obj #js [])

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
  ([{:keys [obj kind hook-name] :as o}]
   (if-not obj
     (swap! all-hooks
            (fn [hs]
              (->> (map (fn [[k v]]
                          [k (remove (fn [{:keys [obj]}]
                                       (= obj o))
                                     v)]) hs)
                   (into {}))))
     (if hook-name
       (hook- obj kind hook-name)
       (hook- obj kind)))
   :ok)
  ([robj rkind]
   (swap! all-hooks update rkind
          #(filter (fn [{:keys [obj kind]}]
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

(def paused false)

(defonce debug-o nil)

(def queued-hooks {})
(def queued-data {})

(defn pause-timer!
  [n]
  (set! paused true)
  (js/setTimeout #(set! paused false) n))

(comment
  (doseq [h (get @all-hooks kind)]
    (try
      (if data
        (run h data)
        (run h))
      (catch js/Error e
        (save :hook-error)
        #_(hook- h)
        (throw e)))
    (when (some-> h :opts :once)
      (hook- h))
    
    (when paused
      
      ))
  )

(defn validate-obj!
  [o]
  (when (or (js/isNaN (.. o -rotation -x))
            (js/isNaN (.. o -position -x)))
    (save :tnhaeo)
    ;;(set! hooks/paused true)
    (throw (js/Error. "FFS hook"))))

(defn run-hooks!
  ([kind]
   (run-hooks! kind nil))
  ([kind data]
   (when-not paused
     (let [data
           (if-let [qd (queued-data kind)]
             (do (set! queued-data (dissoc queued-data kind))
                 qd)
             data)]
       (loop [[h & hs] (if-let [qh (queued-hooks kind)]
                         (do (set! queued-hooks (dissoc queued-hooks kind))
                             qh)
                         (get @all-hooks kind))]
         (when h
           (try
             (if data
               (run h data)
               (run h))
             (when-let [o debug-o]
               (validate-obj! o))
             (catch js/Error e
               (println "Error in hook:" h)
               (set! paused true)
               (save :hook-error)
               #_(hook- h)
               (throw e)))
           (when (some-> h :opts :once)
             (hook- h))
           
           (if paused
             (do (set! queued-hooks (assoc queued-hooks kind hs))
                 (set! queued-data  (assoc queued-data  kind data)))
             (recur hs))))))))

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
