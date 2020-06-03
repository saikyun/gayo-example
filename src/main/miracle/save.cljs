(ns miracle.save
  (:require [clojure.pprint]
            [clojure.string :as str]))

#_(defn eval-in-context
    "Evals the form with the local bindings that have been saved using `save` with `id` as parameter."
    [form bindings id]
    `(let [~@(apply concat (for [[k _] bindings]
                             [k `(-> @miracle.save/saves2 (get ~id) last second (get '~k))]))]
       ~form))

(defonce id (atom 0))

(defn gen-id [] (swap! id inc))

(def ^:dynamic *max-saves* "The maximum number of saves per id." 100)

(defn inspect-map
  [map-to-print & {:keys [desired-level safe-count]
                   :or {desired-level 4 safe-count 10}}]
  (binding [*print-level* desired-level *print-length* safe-count]
    (clojure.pprint/pprint map-to-print)))

(defn gensym? [s]       
  (re-find #"__\d+" s))

(defonce saves2 (atom {}))

(defn eval-in-context
  "Evals the form with the local bindings that have been saved using `save` with `id` as parameter."
  ([form id]
   (eval-in-context 
    form 
    id 
    (-> @miracle.save/saves2 (get id) last first)))
  ([form id pos]
   (let [locals (second
                 (first
                  (filter #(= (first %) pos)
                          (-> @miracle.save/saves2 (get id)))))
         ks (keys locals)]
     `(let [~'all (second (first (filter #(= (first %) ~pos) (-> @miracle.save/saves2 (get ~id)))))
            ~@(apply concat (for [k ks] [k `(get ~'all '~k)]))]
        ~form))))

(defn clear-saves! [] (reset! saves2 {}))

(defn new*
  "Creates a new save at `id`, with `bindings` containing the local bindings from where `save` was called."
  ([ref id]
   (new* id {}))
  ([ref id bindings]
   (swap! ref update id
          (fn [saves]
            (-> (conj saves2 bindings)
                (#(if (< (count %) *max-saves*)
                    (into '() (reverse (take *max-saves* %)))
                    %)))))))

(defn save-fn
  ([ref bindings id]
   (let [bindings `(into {} (remove #(gensym? (name (key %))) ~bindings))]
     `(new* ~ref ~id ~bindings))))

(defn get-last
  [id]
  (let [res (-> @saves2 (get id) last)]
    [(first res) (with-out-str (inspect-map (second res)))]))

(defn get-last-nof
  [id nof]
  (for [res (take nof (reverse (-> @saves2 (get id))))]
    [(first res) (pr-str (subs (with-out-str (inspect-map (second res))) 0 250))]))

(defn print-saves
  [id]
  (let [locals (take 50 (get @saves2 id))]
    (doseq [i (reverse (range (count locals)))
            :let [[k v] (first (drop i locals))]]
      (println "Entry id." k)
      (inspect-map v)
      (prn))))

(comment
  ;; Example usage of basic `save`
  (defn add [x y] (save :a) (+ x y))
  (add 5 10)
  (ld :a)
  x ;; 5
  y ;; 10
  
  (add 20 30)
  (add 7 13)
  
  (print-saves :a))



(comment
  ;; Example usage of `save-var*`
  (do
    (defn yo ([x] x) ([x {:keys [a b]}] (+ x a b)) ([x y & rest] (apply + x y rest)))
    (def wat (partial yo 5))  
    (save-var* 'yo)  
    (save-var* 'wat)  
    (reset! f-saves {})
    (yo 5)
    (wat 5 10)
    (yo 5 {:a 20 :b 30})
    (yo 5 30 40)
    (yo 5 30 40 50)
    @f-saves)

  (unsave-var* 'yo))
