(ns gayo.delay-execution
  (:require [promesa.core :as p]
            [gayo.log :refer [log!]]
            #?(:cljs [miracle.save :refer-macros [save save-do]])))

(defonce execution-queue #queue [])

(defn promise?
  [v]
  (instance? js/Promise v))

(defn later!
  [f]
  (let [p (p/deferred)]
    (set! execution-queue (conj execution-queue [f p]))
    p))


(def done 0)

(defn resolve-all!
  [v]
  (println "managed to do " done)
  (set! done 0)
  (try
    (loop [[fp & fps] execution-queue]
      (let [p (second fp)]
        (when p
          (p/resolve! p v)
          (let [popped (pop execution-queue)]
            (set! execution-queue popped)
            (recur popped)))))
    
    (catch js/Error e
      (log! "error when res all")
      (save :err-res-all)
      (throw e))))

(def all-zero! #(resolve-all! 0))
(def all-nil! #(resolve-all! nil))

(defn update!
  []
  (when-let [[f p] (peek execution-queue)]
    (set! execution-queue (pop execution-queue))
    (let [v (f)]
      (save :watter-updt)
      (set! done (inc done))
      (if (promise? v)
        (p/then v #(p/resolve! p %))
        (p/resolve! p v)))
    
    #_    (try
            (f)
            (catch js/Error e
              (log! "delay fail")
              (save :delay-fail)))))


(comment
  (do
    (-> (later! #(vector 1 2 3))
        (p/then log!))
    (update!))
  
  (p/then [1 2 3] log!)
  
  execution-queue
  (all-zero!)
  
  
  (let [p (p/create (fn [res err]
                      (set! execution-queue
                            (conj execution-queue
                                  #(let [v (+ 1 1)]
                                     (if (promise? v)
                                       (p/then v res)
                                       (res v)))))))]
    (p/then p #(log! "lul123" %))
    #_    (p/resolve! p 123123))
  
  )
