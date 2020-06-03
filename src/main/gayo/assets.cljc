(ns gayo.assets
  (:require ["three" :as THREE]
            [gayo.three-utils :as three-utils]
            [promesa.core :as p]
            [gayo.path :refer [path]]
            
            [clojure.spec.alpha :as s]
            [miracle.save :refer-macros [save]])
  (:require-macros  [gayo.asset-macros :refer [assets-to-map]]))

#?(:browser (set! js/require identity))

;; 
(def ^{:doc "Create the needed `js/require`-statements."}
  assets* (assets-to-map "assets" "../../../"))

(def ^{:doc "
Turn it into something more readable
These will be used throughout the app
E.g. gltf loader will refer to the keys, i.e. \"simple names\"
The values are different depending on the target"}
  assets (into {} (map (fn [[k v]] [k (path v)]) assets*)))

(def ^{:doc "
Does some tricks to the url inputted to the texture loader.
Necessary to go from gltf-paths to the simple names in the `assets` map.
"}
  texture-loader
  #js {:innerLoader (three-utils/TextureLoader.)
       :load (fn [url on-load on-progress on-error]
               (let [asset-url (str "../assets/" url)]
                 (if (assets url)
                   (.load (.-innerLoader texture-loader) (assets* url) #(apply on-load %&) on-progress on-error)
                   (let [err (js/Error. (str "Asset " asset-url " isn't preloaded. Did you forget to `js/require` it?"))]
                     (if on-error
                       (on-error err)
                       (throw err))))))})

(def file-loader (THREE/FileLoader.))

(defn load-texture
  "Takes an asset-name, in relation to the assets-folder, returns a promise that will resolve a `THREE/Texture` object..
  E.g. `(load-texture \"a.png\")` would load `./assets/a.png`."
  ([asset-name]
   (load-texture asset-name nil))
  ([asset-name progress-f]
   (p/create
    (fn [resolve reject]
      (.load texture-loader asset-name resolve progress-f reject)))))

(defn load-asset-file
  ([asset-name]
   (load-asset-file asset-name nil))
  ([asset-name progress-f]
   (p/create
    (fn [resolve reject]
      (.load file-loader asset-name resolve progress-f reject)))))
