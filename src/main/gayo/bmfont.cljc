(ns gayo.bmfont
  (:require ["three" :as THREE]
            [gayo.log :refer [log!]]
            [gayo.assets :as a]
            ["threestuff/msdf" :as MSDFShader]
            ["threestuff/msdf_sprite" :as MSDFSpriteShader]
            [clojure.pprint :refer [pprint pp]]
            [clojure.spec.alpha :as s]
            [promesa.core :as p]
            [cljs-bean.core :refer [bean ->clj ->js]]
            
            #?@(:browser [["font.json" :as fnt-json]]))
  (:require-macros [miracle.save :refer [save save-do]]))

(def newline-data #js {:width 0
                       :height 48
                       :xadvance 0
                       :xoffset 0
                       :yoffset 0
                       :char "\n"})

(defn msdf-sprite-shader
  ([] (msdf-sprite-shader nil))
  ([opts]
   (let [opts (or opts #js {})
         opacity (if (= (type (.-opacity opts)) js/Number)
                   (.-opacity opts)
                   1)
         alphaTest (if (= (type (.-alphaTest opts)) js/Number)
                     (.-alphaTest opts)
                     0.0001)
         precision (or (.-precision opts) "highp")
         color (.-color opts)
         map (.-map opts)
         negate (if (= (type (.-negate opts)) js/Boolean)
                  (.-negate opts)
                  true)]
     
     #js 
     {:uniforms
      #js {:opacity #js { :type "f", :value opacity }
           :map     #js { :type "t", :value (or map (THREE/Texture.)) }
           :color   #js { :type "c", :value (THREE/Color. color) }}
      :extensions #js {:derivatives true}
      :vertexShader
      "attribute vec2 uv;
           attribute vec4 position;
           uniform mat4 projectionMatrix;
           uniform mat4 modelViewMatrix;
           varying vec2 vUv;

           void main() {


  vec2 scale = vec2(
    length(modelViewMatrix[0]),
    length(modelViewMatrix[1])
  );


             vUv = uv;
             //gl_Position = projectionMatrix * modelViewMatrix * position;


gl_Position = projectionMatrix
               * (modelViewMatrix * vec4(0.0, 0.0, 0.0, 1.0)
               + vec4(scale * position.xy, 0.0, 0.0)) * 1.0;

 }"
      :fragmentShader
      (str
       "#ifdef GL_OES_standard_derivatives
             #extension GL_OES_standard_derivatives : enable
           #endif
           precision ", precision, " float;
           uniform float opacity;
           uniform vec3 color;
           uniform sampler2D map;
           varying vec2 vUv;

           float median(float r, float g, float b) {
             return max(min(r, g), min(max(r, g), b));
           }

           void main() {
             vec3 sample = ", (if negate "1.0 - " ""), "texture2D(map, vUv).rgb;
             float sigDist = median(sample.r, sample.g, sample.b) - 0.5;
             float alpha = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
             gl_FragColor = vec4(color.xyz, alpha * opacity);"
       (if (= alphaTest 0)
         ""
         (str "if (gl_FragColor.a < ", alphaTest, ") discard;"))
       "}")})))

(defn msdf-shader
  ([] (msdf-shader nil))
  ([opts]
   (let [opts (or opts #js {})
         opacity (if (= (type (.-opacity opts)) js/Number)
                   (.-opacity opts)
                   1)
         alphaTest (if (= (type (.-alphaTest opts)) js/Number)
                     (.-alphaTest opts)
                     0.0001)
         precision (or (.-precision opts) "highp")
         color (.-color opts)
         map (.-map opts)
         negate (if (= (type (.-negate opts)) js/Boolean)
                  (.-negate opts)
                  true)]
     
     #js 
     {:uniforms
      #js {:opacity #js { :type "f", :value opacity }
           :map     #js { :type "t", :value (or map (THREE/Texture.)) }
           :color   #js { :type "c", :value (THREE/Color. color) }}
      :extensions #js {:derivatives true}
      :vertexShader
      "attribute vec2 uv;
           attribute vec4 position;
           uniform mat4 projectionMatrix;
           uniform mat4 modelViewMatrix;
           varying vec2 vUv;

           void main() {
             vUv = uv;
             gl_Position = projectionMatrix * modelViewMatrix * position;
           }"
      :fragmentShader
      (str
       "#ifdef GL_OES_standard_derivatives
             #extension GL_OES_standard_derivatives : enable
           #endif
           precision ", precision, " float;
           uniform float opacity;
           uniform vec3 color;
           uniform sampler2D map;
           varying vec2 vUv;

           float median(float r, float g, float b) {
             return max(min(r, g), min(max(r, g), b));
           }

           void main() {
             vec3 sample = ", (if negate "1.0 - " ""), "texture2D(map, vUv).rgb;
             float sigDist = median(sample.r, sample.g, sample.b) - 0.5;
             float alpha = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
             gl_FragColor = vec4(color.xyz, alpha * opacity);"
       (if (= alphaTest 0)
         ""
         (str "if (gl_FragColor.a < ", alphaTest, ") discard;"))
       "}")})))

(def default-opts-clj {:scale 3
                       :pos nil
                       :color 0xffffff
                       :maxWidth nil})

(def default-opts (clj->js default-opts-clj))

(defn basic-mesh
  [shader vs uvs]
  (let [geom (THREE/BufferGeometry.)
        y 0.140625
        x 0.125
        s shader
        mat (THREE/RawShaderMaterial. s)
        mesh (THREE/Mesh. geom mat)]
    (.setAttribute geom "position" (THREE/BufferAttribute. vs 3))
    (.setAttribute geom "uv" (THREE/BufferAttribute. uvs 2))
    mesh))

(defn glyph-data*
  [parsed-fnt glyph]
  (let [data (->> (.-chars parsed-fnt)
                  (filter #(= (str (.-char %)) glyph))
                  first)]
    (cond
      (and (not data) (= glyph " ")) 
      #js {:width 16
           :height 48
           :xadvance 11
           :xoffset 0
           :yoffset 0
           :char " "}
      
      (and (not data) (= glyph "\n"))
      newline-data
      
      (not data)
      (log! "missing data for glyph" glyph)
      
      :else
      data)))

(def glyph-data (memoize glyph-data*))

(defn glyph-positions
  [glyph-attrs]
  (reduce
   (fn [{:keys [poses x y]} g]
     {:poses (conj poses {:x x, :y y})
      :x (+ x (/ (.-xadvance g)))
      :y y})
   {:poses []
    :x 0
    :y 0}
   glyph-attrs))

(defn width-of-char
  [width kernings g]
  (let [res (if (nil? g)
              0
              (let [w (/ (.-width g) width)
                    c (.-char g)
                    index
                    (if (.-index g)
                      (js/parseInt (.-index g))
                      nil)
                    org-kerning
                    (if index (js/parseInt (.-amount (nth kernings index))) 0)
                    kerning (/ org-kerning width)
                    xoffset (/ (.-xoffset g) width)
                    x xoffset
                    next-x (- (+ x (/ (.-xadvance g) width) kerning) xoffset)]
                next-x))]
    (when (js/isNaN res)
      (save :widther-of-charer-nan))
    (when (= (some-> g .-char) "\n")
      (save :widther-of-charer-wat123))
    res))

(s/def ::poses (s/coll-of (s/coll-of number?)))
(s/def ::layout-width number?)
(s/def ::x number?)
(s/def ::y number?)
(s/def ::glyph-poses (s/keys :un-req [::poses ::layout-width ::x ::y]))

(defn glyph-poses*
  [common kernings glyph-attrs max-w width height]
  (let [line-height (/ (.-lineHeight common) height)]
    (loop [i 0
           res {:poses []
                :x 0
                :layout-width 0
                :y 0}]
      (if (>= i (count glyph-attrs))
        res
        (let [{:keys [poses x y layout-width]} res
              g (nth glyph-attrs i)
              word-l
              (try
                (reduce
                 +
                 (map
                  (partial
                   width-of-char
                   width
                   kernings)
                  (take-while #(not (#{"\n" " "} (some-> % .-char))) (drop i glyph-attrs))))
                (catch js/Error e
                  (save :width-error)
                  (throw e))
                )]
          #_          (log! "current" (.-char g) "word-l" (map #(.-char %) (take-while #(not (#{"\n" " "} (.-char %))) (drop i glyph-attrs))))
          
          (if (and max-w
                   (< max-w (+ x word-l)))
            
            (recur i
                   {:poses poses
                    :layout-width layout-width
                    :x 0
                    :y (- y line-height)})
            
            (let [w (/ (.-width g) width)
                  c (.-char g)
                  h (/ (.-height g) height)
                  index (if (.-index g)
                          (js/parseInt (.-index g))
                          nil)
                  org-kerning (if index (js/parseInt (.-amount (nth kernings index))) 0)
                  kerning (/ org-kerning width)
                  base (/ 36 height)
                  yoffset (/ (.-yoffset g) height)
                  xoffset (/ (.-xoffset g) width)
                  y2 (+ base (- y yoffset h))
                  x (+ x xoffset)]
              (let [newline (or (and max-w 
                                     (< max-w
                                        (+ x xoffset)))
                                (= c "\n"))
                    pot-next-x (- (+ x (/ (.-xadvance g) width) kerning) xoffset)
                    next-x (if newline
                             0
                             pot-next-x)]
                (when (= c "\n")
                  (save :new1))
                (recur (inc i)
                       {:poses (conj poses
                                     [(+ x w) (+ y2 h) 0
                                      x (+ y2 h) 0
                                      x y2 0
                                      
                                      x y2 0
                                      (+ x w) y2 0
                                      (+ x w) (+ y2 h) 0])
                        :layout-width (max pot-next-x layout-width)
                        :x next-x
                        :y (if newline
                             (- y line-height)
                             y)}))))))))
  )

(def glyph-poses glyph-poses* #_ (memoize glyph-poses*))

(defn glyph-uvs*
  ([glyph-attrs width height]
   (reduce
    (fn [uvs g]
      (if (and (.-x g) (.-y g))
        (let [x (/ (.-x g) width) 
              y (- 1 (/ (.-y g) height)) 
              w (/ (.-width g) width) 
              h (/ (.-height g)
                   height)]
          (conj uvs
                [(+ x w) y
                 x y
                 x (- y h)
                 
                 x (- y h) 
                 (+ x w) (- y h) 
                 (+ x w) y]))
        (conj uvs
              [0 0
               0 0
               0 0
               
               0 0
               0 0
               0 0])))
    []
    glyph-attrs))
  ([glyph-attrs glyphs width height]
   (js/Float32Array.
    (clj->js 
     (flatten (reduce
               (fn [uvs g]
                 (let [g (get glyph-attrs g)]
                   (if (and (.-x g) (.-y g))
                     (let [x (/ (.-x g) width) 
                           y (- 1 (/ (.-y g) height)) 
                           w (/ (.-width g) width) 
                           h (/ (.-height g)
                                height)]
                       (conj uvs
                             [(+ x w) y
                              x y
                              x (- y h)
                              
                              x (- y h) 
                              (+ x w) (- y h) 
                              (+ x w) y]))
                     (conj uvs
                           [0 0
                            0 0
                            0 0
                            
                            0 0
                            0 0
                            0 0]))))
               []
               glyphs))))))

(def glyph-uvs (memoize glyph-uvs*))

#?(:rn (def fnt-json (js/require "../../../assets/font.json")))
(def parsed-fnt fnt-json)
(def width (-> parsed-fnt .-common .-scaleW))
(def height (-> parsed-fnt .-common .-scaleH))

(defn init-text
  []
  (p/let [texture (a/load-texture "font.png")
          shader (msdf-sprite-shader
                  #js {:map texture
                       :color 0xffffff
                       :transparent true
                       :opacity 1
                       :alphaTest 0.5
                       :negate false})]
    [shader texture]))

(defn create-text*
  [shader text-texture text max-ani opts]
  (let [glyphs text
        gd (map #(glyph-data parsed-fnt %) glyphs)
        poses (glyph-poses (.-common parsed-fnt)
                           (.-kernings parsed-fnt)
                           gd
                           (when-let [mw (and (not (.-scaleDown opts))
                                              (.-maxWidth opts))]
                             (/ mw (.-scale opts)))
                           width
                           height)
        pre-uv (flatten (glyph-uvs gd width height))
        uv (js/Float32Array. (clj->js pre-uv))
        pre-vs (flatten (:poses poses))
        half-w (* 0.5 (:layout-width poses))
        pre-vs (map-indexed 
                #(if (= 0 (mod %1 3))
                   (- %2 half-w)
                   %2) pre-vs)
        vs (js/Float32Array. (clj->js pre-vs))]
    (def poss poses)
    (def hh half-w)
    (def uuu uv)
    (def vvv vs)
    (def pv pre-vs)
    (def pu pre-uv)
    
    (if (and max-ani text-texture)
      (do
        (set! (.-minFilter text-texture) THREE/LinearMipMapLinearFilter)
        (set! (.-magFilter text-texture) THREE/LinearFilter)
        (set! (.-generateMipmaps text-texture) true)
        
        (set! (.-anisotropy text-texture) max-ani)
        (set! (.-needsUpdate text-texture) true))
      (log! "No max-ani set."))
    
    (let [;;base (THREE/Geometry.)
          x-offset (* 0.5 (:x poses))
          texture-height (-> parsed-fnt .-common .-scaleH
                             (js/parseInt))
          line-base (-> parsed-fnt .-common .-base
                        (js/parseInt))
          line-height (-> parsed-fnt .-common .-lineHeight
                          (js/parseInt))
          y-offset (+ (* 0.5 (/ line-height texture-height))
                      (:y poses))
          
          ;;          uv (nth uvs i) 
          ;;         vs (js/Float32Array. (clj->js (nth (:poses poses) i)))
          color (.-color opts)
          color (or color 0x000000)
          flat (.-flat opts)
          shader (if flat
                   (msdf-sprite-shader
                    #js {:map text-texture
                         :color color
                         :transparent true
                         :opacity 1
                         :alphaTest 0.5
                         :negate false})
                   (msdf-shader
                    #js {:map text-texture
                         :color color
                         :transparent true
                         :opacity 1
                         :alphaTest 0.5
                         :negate false}))
          base (basic-mesh shader vs uv)]
      
      (save :coo)
      
      (set! (.. base -offset) (THREE/Vector3. (- x-offset) 0 0))
      (.. base -rotation (set (* 0.5 (- js/Math.PI)) 0 0))
      #_      (.. base -position (set 4 3 3))
      
      (set! (.. base -userData -name) "yeah123")
      
      (def bbb base)
      (set! (.-textContent base) text)
      
      (set! (.-textWidth base) (:x poses))
      (set! (.-textHeight base) y-offset)
      (set! (.-yOffset base) y-offset)
      
      base)))

(comment
  (set! (bean (.. test.internal/fps-text -geometry -attributes -uv)) nil)
  (bean (.. test.internal/fps-text -geometry))
  (bean (.. test.internal/fps-text -geometry (getAttribute "uv")))
  
  (THREE/BufferAttribute. lol 1)
  
  (.. test.internal/fps-text -position (set -1 1 1))
  (time  (update-text* test.internal/fps-text "2"))
  )

(def gd (into {} (map #(vector % (glyph-data parsed-fnt %)) "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖabcdefghijklmnopqrstuvwxyzåäö 0123456789 -':+.,")))

(defn update-text*
  [mesh text]
  (let [uv (glyph-uvs gd text width height)]
    (.. mesh -geometry (setAttribute "uv" (THREE/BufferAttribute. uv 2)))
    
    mesh))

(comment
  
  (.remove gayo-data/scene lol)
  (def lol (create-text* test.game/shader "hello" 16))
  (.add gayo-data/scene lol)
  (.. lol -rotation (set (* 0.5 (- js/Math.PI)) 0 0))
  (.. gayo.data/lol -rotation (set (* 0.5 (- js/Math.PI)) 0 0))
  
  (.. lol -position (set 1 1 1))
  
  )

(def get-text create-text* #_(memoize create-text*))

(defn create-text!
  ([shader text-texture text]
   (create-text! shader text-texture text nil))
  ([shader text-texture text max-ani]
   (create-text! shader text-texture text max-ani default-opts))
  ([shader text-texture text max-ani opts]
   (get-text shader text-texture text max-ani opts)))
