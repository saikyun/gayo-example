(ns miracle.soar
  (:require [user]
            ["three" :as THREE]
            [gayo.state :as state :refer [state update-state!]]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs-bean.core :refer [->clj]]
            
            [miracle.save :refer-macros [save]]))

(defn get-or-create
  ([id] (get-or-create "div" id))
  ([tag id]
   (or (.getElementById js/document id)
       (let [app (.createElement js/document tag)]
         (set! (.-id app) id)
         (.. js/document -body (appendChild app))
         app))))

(defonce div (get-or-create "miracle.soar"))
(set! (.. div -style -float) "left")
(defonce debugging false)

(defn view
  []
  [:div {:style {:position :absolute
                 :left 0
                 :top 0}}
   "hej"])

(defn node
  [tag {:keys [style id]} & children]
  (let [n (.. js/document (createElement (name tag)))]
    (set! (.. n -id) id)
    (doseq [[k v] style]
      (aset (.. n -style) (name k) (if (keyword? v)
                                     (name v)
                                     v)))
    (doseq [c children]
      (if (seq? c)
        (doseq [c2 c]
          (.append n c2))
        (.append n c)))
    n))

(defonce current-nodes {})

(defn data-nodes
  [hmap]
  (let [nodes (for [[k v] hmap]
                (let [v-node (node :div {} (with-out-str (pr v)))]
                  [k v-node (node :div {} (str k) " " v-node)]))]
    (set! current-nodes (into {} (map (juxt first second) nodes)))
    (map #(nth % 2) nodes)))

(defn update-node!
  [k v]
  (if-let [node (current-nodes k)]
    (set! (.-innerHTML node) (str v))
    (println "implement inserting new nodes...")))

(defonce watching (r/atom nil))

(declare render start)

(defn watch
  [o]
  (when (state/watch o)
    (start))
  (reset! watching (.-state o))
  ;;(reset! watching {:a (rand 123)})
  #_(when-not (identical? o watching)
      (set! watching o)
      (render)))

(defn debug
  [o & extra]
  (watch o)
  
  (macroexpand '(state watched-o :org-scale))
  
  ;;(user/only-render)
  ;;(js-debugger)
  )

(def selected-field (r/atom nil))
(def selected-value (r/atom nil))

(defn input-vector3
  [k v]
  [:<> [:input {:on-focus #(do (reset! selected-field [k :x])
                               (println "sel field" @selected-field)
                               (reset! selected-value (.-x v)))
                :on-change
                (fn [ev]
                  (println v)
                  (let [v (.. ev -target -value)]
                    (save :tnshaoe)
                    (reset! selected-value v)
                    (when-not (js/isNaN v)
                      (println "lul" v)
                      
                      (goog.object/set (.-state @state/watched-o)
                                       (name k)
                                       "x" v))))
                :value (if (= @selected-field [k :x])
                         (do (println "selcted")
                             @selected-value)
                         (.-x v))}]])

(defn default-input
  [k v]
  [:div (pr-str v)])

(defn input
  [k v]
  [:<> (condp = (type v)
         THREE/Vector3 [input-vector3 k v]
         [default-input k v])])

(defonce opened (r/atom false))

(defn intro
  []
  [:div {:style {:background-color "rgb(89, 0, 90)"
                 :color "rgba(255, 255, 255, 0.9)"
                 :width "300px"
                 :padding "4px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :grid-gap "10px"}}
    [:h6 "Watcher"]
    [:button {:on-click #(swap! opened not)} (if @opened "Hide" "Show")]]
   (when-let [o (and @opened state/watched-o)]
     (into [:div {:style {:display "grid"
                          :width "300px"
                          :grid-template-columns "repeat(2, fit-content(20%))"
                          :grid-gap "10px"}}]
           (for [[k v] @(.-state o)]
             [:<>
              [:div (pr-str k)]
              [input k v]])))])

(defn render
  []
  #_(set! (.-innerHTML div) "")  
  #_(.append div (node :div
                       {:style {:background-color "rgb(89, 0, 90)"
                                :color "rgba(255, 255, 255, 0.9)"
                                :width "300px"
                                :padding "4px"}}
                       (data-nodes (->clj watching))))
  
  )

(defn ^:dev/after-load start []
  (set! debugging true)
  (rdom/render [intro] div))

(when debugging
  (render))
