(ns gayo.state
  (:require [clojure.string :as str]))

(def watched-o nil)
(def last-ten-order nil)
(defn debug [])
(defn debug-reg [])

(defn camel-case
  [s]
  (-> (str/replace s #"-([A-Za-z])" (fn [[_ char]]
                                      (str/upper-case char)))
      (str/replace #"-" "")))

(defn convert-name
  [s]
  (if-let [ns (namespace s)]
    (camel-case (str ns "___" (name s)))
    (camel-case (name s))))

(defn attr
  [o & ks]
  `(.. ~o ~'-state ~@(map #(symbol (str "-" (convert-name %)))
                          ks)))

(defmacro state
  ([o]
   `(if (= (type (~'.-state ~o)) reagent.ratom/RAtom)
      (assoc (deref (~'.-state ~o))
             :name (~'.. ~o ~'-userData ~'-name)
             :rotation (~'.-rotation ~o)
             :position (~'.-position ~o))
      (assoc (cljs-bean.core/->clj (~'.-state ~o))
             :name (~'.. ~o ~'-userData ~'-name)
             :rotation (~'.-rotation ~o)
             :position (~'.-position ~o))))
  ([o k]
   `(if (identical? ~o ~'gayo.state/watched-o)
      (~k @(~'.-state ~o))
      ~(attr o k)))
  ([o k & ks]
   (concat `(-> (state ~o ~k))
           (map (fn [k] `(state ~k)) ks))))

(comment
  (macroexpand '(state o :a :b :c))
  
  (macroexpand '(state o :a :b))
  )

(defmacro state+
  ([o k v]
   `(if (identical? ~o ~'gayo.state/watched-o)
      (let [v# ~v]
        (do (debug ~o ~k v# ~(meta &form))
            (swap! (~'.-state ~o) assoc ~k v#)
            v#))
      (set! ~(attr o k) ~v)))
  ([o k1 v1 & kvs]
   `(do (state+ ~o ~k1 ~v1)
        ~@(for [[k v] (partition 2 kvs)]
            `(state+ ~o ~k ~v)))))

(defmacro update-state!
  [o k f & args]
  `(if (identical? ~o ~'gayo.state/watched-o)
     (let [~'v (apply ~f (~k @(~'.-state ~o)) ~(vec args))]
       (debug ~o ~k ~'v ~(meta &form))
       (swap! (~'.-state ~o) assoc ~k ~'v)
       ~'v)
     (state+ ~o ~k (apply ~f ~(attr o k) ~(vec args)))))

(comment
  (gayo.state/attr 'o :damage)
  
  (macroexpand '(state+ o :a 10, :b 20, :c 30))
  
  )
