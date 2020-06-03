(ns miracle.save)

(def enabled true)

(defmacro save
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [key]
  (when enabled
    `(do (swap! miracle.save/saves2
                update
                ~key
                #(into []
                       (take-last 
                        *max-saves*
                        (conj %
                              [(gen-id)
                               (into {} (list ~@(let [ks (keys (:locals &env))]
                                                  (for [k ks]
                                                    `['~k ~k]))))]))))
         :ok)))

(defmacro save-do
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [key & body]
  (if enabled
    `(let [~'_ (swap! miracle.save/saves2
                      update
                      ~key
                      #(into []
                             (take-last 
                              *max-saves*
                              (conj %
                                    [(gen-id)
                                     (into {} (list ~@(let [ks (keys (:locals &env))]
                                                        (for [k ks]
                                                          `['~k ~k]))))]))))]
       ~@body)
    `(do
       ~@body)))
