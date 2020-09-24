(ns gayo.printers
  (:require ["three" :as THREE]
            [goog.string :as gstring]
            [goog.string.format]))

(extend-type THREE/Vector3
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Vector3[")
    (-write writer (gstring/format "%.2f %.2f %.2f" (.-x a) (.-y a) (.-z a)))
    (-write writer "]")))

(extend-type THREE/Box3
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Box3[")
    (let [min (.-min a)
          max (.-max a)]
      (-write writer (str "[" (.-x min) " " (.-y min) " " (.-z min) "], "))
      (-write writer (str "[" (.-x max) " " (.-y max) " " (.-z max) "]")))
    (-write writer "]")))

(extend-type THREE/Raycaster
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Raycaster[")
    (when-let [dir (.-direction a)]
      (-write writer (gstring/format "%.2f %.2f %.2f" (.-x dir) (.-y dir) (.-z dir))))
    (-write writer "]")))

(extend-type THREE/Euler
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Euler[")
    (-write writer (str (.-x a) " " (.-y a) " " (.-z a)))
    (-write writer "]")))

(extend-type THREE/Quaternion
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Quaternion[")
    (-write writer (str (.-x a) " " (.-y a) " " (.-z a) " " (.-w a)))
    (-write writer "]")))

(extend-type THREE/Vector2
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Vector2[")
    (-write writer (str (.-x a) " " (.-y a)))
    (-write writer "]")))

(extend-type THREE/Vector4
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Vector4[")
    (-write writer (str (.-x a) " " (.-y a) " " (.-z a) " " (.-w a)))
    (-write writer "]")))

(extend-type THREE/Mesh
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Mesh[")
    (if-let [n (or (some-> a .-userData .-name) (.-name a))]
      (-write writer (pr-str n))
      (-write writer (pr-str "Unnamed")))
    (-write writer "]")))

(extend-type THREE/Object3D
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#Object3D[")
    (if-let [n (or (some-> a .-userData .-name) (.-name a))]
      (-write writer (pr-str n))
      (-write writer (pr-str "Unnamed")))
    (-write writer "]")))

(extend-type THREE/PerspectiveCamera
  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#PerspectiveCamera[")
    (if-let [n (or (some-> a .-userData .-name) (.-name a))]
      (-write writer (pr-str n))
      (-write writer (pr-str "Unnamed")))
    (-write writer "]")))

