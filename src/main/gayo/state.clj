(ns gayo.state
  (:require [clojure.string :as str]))

(defn camel-case
  [s]
  (-> (str/replace s #"-(.)" (fn [[_ char]]
                               (str/upper-case char)))
      (str/replace #"-" "")))

(defn convert-name
  [s]
  (if-let [ns (namespace s)]
    (camel-case (str ns "___" (name s)))
    (camel-case (name s))))

(defn attr
  [o k]
  `(.. ~o ~'-state ~(symbol (str "-" (convert-name k)))))

(defmacro state
  [o k]
  (attr o k))

(defmacro upd!
  [o k f & args]
  (let [a (attr o k)]
    `(let [v# ~a]
       (set! ~a (apply ~f v# ~args)))))
