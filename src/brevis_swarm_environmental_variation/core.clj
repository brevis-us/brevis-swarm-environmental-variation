#_"This file is a Brevis contribution.
                                                                                                                                                                                     
    brevis is free software: you can redistribute it and/or modify                                                                                                           
    it under the terms of the GNU General Public License as published by                                                                                                             
    the Free Software Foundation, either version 3 of the License, or                                                                                                                
    (at your option) any later version.                                                                                                                                              
                                                                                                                                                                                     
    brevis is distributed in the hope that it will be useful,                                                                                                                
    but WITHOUT ANY WARRANTY; without even the implied warranty of                                                                                                                   
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                                                                                                    
    GNU General Public License for more details.                                                                                                                                     
                                                                                                                                                                                     
    You should have received a copy of the GNU General Public License                                                                                                                
    along with brevis.  If not, see <http://www.gnu.org/licenses/>.                                                                                                          
                                                                                                                                                                                     
Copyright 2012, 2014 Kyle Harrington"

(ns brevis-swarm-environmental-variation.core
  (:gen-class)
  (:import [org.lwjgl.util.vector Vector3f Vector4f])
  (:use [brevis.vector]
        [brevis.graphics.basic-3D]
        [brevis.physics collision core space utils]
        [brevis.shape box sphere cone]
        [brevis core osd random input globals display parameters math
         plot]        
        [clojure.set])
  (:require [clojure.string :as string]))

; If you base research upon this simulation, please reference the following paper:
;
; Lowell, J., K. Harrington, and J. Pollack, (2014) "The Resilience of a Swarm Ecosystem Under Environmental Variation". In Proceedings of Artificial Life XIV, pp. 827-834.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Globals

(reset! params {:num-birds 250
                :initial-bird-energy 1
                :delta-bird-energy 0.0025
                :delta-consumption 0.00005
                :delta-collision 0.00001
                :randomization-rate 0.01
                ;:num-foods 25
                :low-num-foods 5
                :high-num-foods 25
                :initial-food-energy 1
                :delta-food-energy 0.1
                :variation-trigger 20000 ;; if this is a number, then this is a time threshold. otherwise it should be a predicate (some function testable for true/false)
                :terminate-trigger 40000 ;; if this is a number, then this is a time threshold. otherwise it should be a predicate (some function testable for true/false)
                :season-frequency 0.0005
                :initial-environment-type :seasonal ; :uniform-high, :uniform-low, :seasonal-fast, :seasonal-slow, :gaussian-dense, :gaussian-loose
                :final-environment-type :seasonal ; :uniform-high, :uniform-low, :seasonal-high, :seasonal-low, :gaussian-dense, :gaussian-loose
                :environment-type :seasonal ; :uniform-high, :uniform-low, :seasonal-high, :seasonal-low, :gaussian-dense, :gaussian-loose
                :max-acceleration 0.5
                :log-interval nil
                :max-acceleration-gene-value 10
                :start-time (System/nanoTime)
                :output-directory ""
                :dt 0.5
                :gui true
                })

; Note: in the version used in the paper, there was a mismatch between physics dt and energy dt

;; Environment

(def dead-birds (atom 0))
(def accumulated-energy (atom (float 0)))
(def triggered (atom false))

(def speed 25)

(def floor (atom nil))

(def width 400)
(def height width)

(defn log-filename
  []
  (string/replace 
    (str (:output-directory @params) "brevisEnvVarSwarm_nbirds_" (:num-birds @params) 
         "_initenv_" (:initial-environment-type @params) 
         "_finalenv_" (:final-environment-type @params) 
         "_dcollision_" (:delta-collision @params) 
         "_dbirdenergy_" (:delta-bird-energy @params) 
         "_st_" (:start-time @params) ".txt")
    ":" ""))
 
(let [min-x (- width) ;(- (/ width 2))
      max-x width ;(/ width 2)
      min-z (- height) ;(- (/ height 2))
      max-z height ;(/ height 2)
      min-y -10
      max-y 200]
  (defn out-of-bounds
    "Test if a position is out of bounds."
    [p]
    (or (> (.x p) max-x)
        (< (.x p) min-x)
        (> (.z p) max-z)
        (< (.z p) min-z)
        (> (.y p) max-y)
        (< (.y p) min-y))))

(defn log-string
  "Log a string to the current log filename."
  [s]
  (spit (log-filename) s :append true))

(defn env-num-foods
  "Return the proper number of foods for this environmnet type"
  []
  (cond (= (:environment-type @params) :uniform-high) (:high-num-foods @params)
        (= (:environment-type @params) :uniform-low) (:low-num-foods @params)
        (= (:environment-type @params) :seasonal) (Math/floor (+ (* (+ 1 (Math/sin (* (:season-frequency @params) (get-time))))
                                                                    (/ (- (:high-num-foods @params) (:low-num-foods @params)) 2))
                                                                 (:low-num-foods @params)))
        (number? (:environment-type @params)) (:environment-type @params)))        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Foods

(defn food?
  "Is a thing food?"
  [thing]
  (= (get-type thing) :food))

(defn random-food-position
  "Return a random valid location for food."
  []
  (cond (or (not= (:environment-type @params) :gaussian-dense)
            (not= (:environment-type @params) :gaussian-loose));; Everyone is uniformly spaced, unless gaussian
        (vec3 (- (rand width) (/ width 2)) 
              (+ 45 (rand 10))
              (- (rand height) (/ height 2)))
        :else
        (let [t (* 2 Math/PI (lrand (/ width 2)))
              u (+ (lrand (/ width 2)) (lrand (/ width 2)))
              r (if (> u (/ width 2)) (- width u) u)]
          (vec3 (* r (Math/cos t)) 
                (+ 45 (rand 10))
                (* r (Math/sin t))))))
    
(defn make-food
  "Make a food entity."
  [position]
  (move (assoc (make-real {:type :food
                           :color (vec4 0 1 1 1)
                           :shape (create-sphere 5)})
               :energy (:initial-food-energy @params) #_@initial-food-energy)
        position))

(defn random-food
  "Make a food at a random position."
  []
  (make-food (random-food-position)))

(defn update-food
  "Update a food item."
  [food]
  (if (> (:energy food) (* (get-dt) (:delta-consumption @params)#_@delta-consumption));; if this food still has enough energy for a consumption event
    (assoc (set-color food
                      (vec4 (* 1 (:energy food)) 0 1 1))
           :energy (+ (:energy food) (* (get-dt) (:delta-food-energy @params)#_@delta-food-energy)))
    (move (assoc (set-color food
                            (vec4 (* 10 (:energy food)) 0 1 1))
                 :energy (:initial-food-energy @params) #_@initial-food-energy)
          (random-food-position))))
(add-update-handler :food update-food)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Birds

(defn bird?
  "Is a thing a bird?"
  [thing]
  (= (get-type thing) :bird))

(defn random-bird-position
  "Returns a random valid bird position."
  []
  (vec3 (- (rand width) (/ width 2)) 
        (+ 59.5 (rand 10));; having this bigger than the neighbor radius will help with speed due to neighborhood computation
        (- (rand height) (/ height 2))))

(defn random-genome
  "Make a random bird genome."
  []
  {:neighborC (* (:max-acceleration-gene-value @params) (- (lrand 2) 1));; Neighbor coefficient for close behavior
   :neighborF (* (:max-acceleration-gene-value @params) (- (lrand 2) 1));; Neighbor coefficient for far behavior
   :neighborD (lrand (java.lang.Math/sqrt (+ (* width width) (* height height))));; Neighbor distance
   :foodC (* (:max-acceleration-gene-value @params) (- (lrand 2) 1));; Food coefficient for close behavior
   :foodF (* (:max-acceleration-gene-value @params) (- (lrand 2) 1));; Food coefficient for far behavior
   :foodD (lrand (java.lang.Math/sqrt (+ (* width width) (* height height))));; food distance
   })

;; unbounded genomes
#_(defn mutate-genome
   "Mutate a bird's genome."
   [genome]
   (let [mut-step 0.5]
     (into {}
           (for [[k v] genome]
             [k (* (+ 1 (lrand (* 2 mut-step)) (- mut-step)) v)]))))

(defn mutate-genome
   "Mutate a bird's genome. Distance is bounded"
   [genome]
   (let [mut-step 0.5
         new-genome (into {}
                          (for [[k v] genome]
                            [k (* (+ 1 (lrand (* 2 mut-step)) (- mut-step)) v)]))]
     (assoc new-genome
            :neighborC (max (- (:max-acceleration-gene-value @params))
                            (min (:neighborC new-genome)
                                 (:max-acceleration-gene-value @params)))
            :neighborF (max (- (:max-acceleration-gene-value @params))
                            (min (:neighborF new-genome)
                                 (:max-acceleration-gene-value @params)))
            :neighborD (min (java.lang.Math/sqrt (+ (* width width) (* height height)))
                            (max 0 (:neighborD new-genome)))
            :foodC (max (- (:max-acceleration-gene-value @params))
                        (min (:foodC new-genome)
                             (:max-acceleration-gene-value @params)))
            :foodF (max (- (:max-acceleration-gene-value @params))
                        (min (:foodF new-genome)
                             (:max-acceleration-gene-value @params)))
            :foodD (min (java.lang.Math/sqrt (+ (* width width) (* height height)))
                        (max 0 (:foodD new-genome))))))

(defn make-bird
  "Make a new bird with the specified program. At the specified location."
  [position]  
  (assoc (move (make-real {:type :bird
                           :color (vec4 (lrand) (lrand) (lrand) 1)
                           ;:color (vec4 1 0 0 1)
                           ;:shape (create-sphere)})
                           :shape (create-cone 2.2 1.5)})
               position)
         :birth-time (get-time)
         :energy (:initial-bird-energy @params) #_@initial-bird-energy
         :genome (random-genome)))
  
(defn random-bird
  "Make a new random bird."
  []
  (make-bird (random-bird-position)))    

(defn bound-acceleration
  "Keeps the acceleration within a reasonable range."
  [v]  
  (if (> (length v) (:max-acceleration @params))
    (mul (div v (length v)) (:max-acceleration @params))
    v))

(defn fly
  "Change the acceleration of a bird."
  [bird]
  (let [nbrs (get-neighbor-objects bird)
        nbr-birds (filter bird? nbrs)
        nbr-foods (filter food? nbrs)
        closest-bird (when-not (empty? nbr-birds) (first nbr-birds))        
        closest-food (when-not (empty? nbr-foods) (first nbr-foods))        
        bird-pos (get-position bird)
        dclosest-bird (when closest-bird (normalize (sub (get-position closest-bird) bird-pos)))
        dclosest-food (when closest-food (normalize (sub (get-position closest-food) bird-pos)))
        new-acceleration (add (if closest-bird
                                (if (< (length dclosest-bird) (:neighborD (:genome bird)))
                                  (mul dclosest-bird (:neighborC (:genome bird)))
                                  (mul dclosest-bird (:neighborF (:genome bird))))
                                (vec3 0 0 0))
                              (if closest-food
                                (if (< (length dclosest-food) (:foodD (:genome bird)))
                                  (mul dclosest-food (:foodC (:genome bird)))
                                  (mul dclosest-food (:foodF (:genome bird))))
                                (vec3 0 0 0)))]
    (if (or (< (:energy bird) 0) (out-of-bounds bird-pos))
      (do (swap! dead-birds inc)
        (move (assoc bird
                     :birth-time (get-time)
                     :energy (:initial-bird-energy @params) #_@initial-bird-energy
                     :genome (let [r (lrand)]
                               (< r (:randomization-rate @params))
                               (random-genome)
                               :else ;; create a mutant baded on a random individual with birth time of not now
                               (mutate-genome (:genome (lrand-nth (filter #(and (bird? %) (not= (:birth-time %) (get-time))) (all-objects)))))))
              (random-bird-position)))
      (assoc (set-acceleration
                bird
                (bound-acceleration
                  new-acceleration))
              :energy (- (:energy bird) (* (get-dt) (:delta-bird-energy @params)))))))

(enable-kinematics-update :bird); This tells the simulator to move our objects
(add-update-handler :bird fly); This tells the simulator how to update these objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Global updates 

(let [log-counter (atom 0)]
  (add-global-update-handler 1 
                              (fn [] (let [objs (all-objects)
                                           foods (filter food? objs)
                                           birds (filter bird? objs)
                                           average-age (/ (apply + (map #(- (get-time) (get % :birth-time)) birds))
                                                          (count birds))
                                           avg-food-energy (/ (apply + (map :energy foods)) (count foods))
                                           avg-bird-energy (/ (apply + (map :energy birds)) (count birds))
                                           all-genetics (map #(list (:neighborC %)
                                                                    (:neighborF %)
                                                                    (:neighborD %)
                                                                    (:foodC %)
                                                                    (:foodF %)
                                                                    (:foodD %)) (map :genome birds))
                                           agg-genetics (apply concat (for [k (range 6)]
                                                                        (let [genes (map #(nth % k) all-genetics)]
                                                                          [(/ (apply + genes) (count birds))
                                                                           (std-dev genes)])))]
                                       (when (and (:log-interval @params)
                                                  (zero? (mod @log-counter (:log-interval @params))))
                                         (log-string (str "" (string/join "\t" 
                                                                          (concat [(get-time) avg-bird-energy
                                                                                   avg-food-energy
                                                                                   @dead-birds
                                                                                   @accumulated-energy
                                                                                   average-age]
                                                                                  agg-genetics
                                                                                  [(count foods)])) "\n")))
                                       (swap! log-counter inc)))))

;; Environmental variation global update handler

(add-global-update-handler 2
                           (fn [] (let [objs (all-objects)
                                        foods (filter food? objs)]                                    
                                    ;; terminate simulation if need be
                                    (when (and (number? (:terminate-trigger @params)) 
                                               (> (get-time) (:terminate-trigger @params)))
                                      (swap! *gui-state* assoc :close-requested true))
                                    ;; switch environment types if need be
                                    (cond (and (not @triggered) 
                                               (number? (:variation-trigger @params)) 
                                               (> (get-time) (:variation-trigger @params)))
                                          (do (reset! triggered true)
                                            (swap! params assoc :environment-type (:final-environment-type @params))))
                                    (loop [foods foods]
                                      (cond (> (count foods) (env-num-foods))
                                            (let [foods (lshuffle foods)
                                                  obj (first foods)]
                                              (del-object obj)
                                              (recur (rest foods)))
                                            (< (count foods) (env-num-foods))
                                            (let [new-food (random-food)]
                                              (add-object new-food)
                                              (recur (conj foods new-food))))))))
                                              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Collision handling
;;
;; Collision functions take [collider collidee] and return [collider collidee]
;;   Both can be modified; however, two independent collisions are actually computed [a b] and [b a].

(defn bump
 "Collision between two birds. This is called on [bird1 bird2] and [bird2 bird1] independently
so we only modify bird1."
 [bird1 bird2]  
 [(assoc bird1 :energy (- (:energy bird1) (:delta-collision @params)))
  bird2])
(add-collision-handler :bird :bird bump)

(defn land
  "Collision between a bird and the floor."
  [bird floor]
  [(set-velocity (set-acceleration bird (vec3 0 0.5 0)) (vec3 0 0.5 0));; maybe move as well       
   floor])

(add-collision-handler :bird :floor land)

;; A bird eats
(add-collision-handler :bird :food 
                       (fn [bird food]
                         (swap! accumulated-energy #(+ % (:delta-consumption @params) #_@delta-consumption))
                         [(assoc bird :energy (+ (:energy bird) (:delta-consumption @params) #_@delta-consumption))]))

;; A food is eaten
(add-collision-handler :food :bird 
                       (fn [food bird]
                         [(assoc food
                                 :energy (- (:energy food) (:delta-consumption @params) #_@delta-consumption)) bird]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## brevis control code

(defn initialize-simulation
  "This is the function where you add everything to the world."
  []  
  (init-world)
  (init-view)
  (swap! brevis.globals/*gui-state* assoc :gui (:gui @params))

  (.setPosition (:camera @brevis.globals/*gui-state*) (vec3 20.0 -235 -400))
  (.setRotation (:camera @brevis.globals/*gui-state*) (vec4 35 0 -90 0))

  (set-dt (:dt @params))
  (set-neighborhood-radius 10000)
  (default-display-text)
  (move-light 1 [1 0 0 1])
  (add-object (make-floor width height))
  
  ;; replace this with an environment setup function
  (dotimes [_ (env-num-foods)#_(:num-foods @params)]
    (add-object (random-food)))
  (dotimes [_ (:num-birds @params)]
    (add-object (random-bird)))
  
  (doseq [gene-name [:neighborC :neighborF :neighborD :foodC :foodF :foodD]]
    (add-plot-handler
      (fn [] 
        (let [genes (map #(gene-name (:genome %))
                         (filter bird? (all-objects)))
              avg-gene (/ (apply + genes) (count genes))]
          [(* (get-time) (get-dt)) avg-gene]))
      :interval 200
      :title (str "Gene " (name gene-name)))))

;; Start zee macheen
(defn -main [& args]       
  (let [;; First put everything into a map                                                                                                                                                                                                                                                                                 
        argmap (apply hash-map
                      (mapcat #(vector (read-string (first %)) (second %) #_(read-string (second %)))
                              (partition 2 args)))
        ;; Then read-string on *some* args, but ignore others                                                                                                                                                                                                                                                              
        argmap (apply hash-map
                      (apply concat
                             (for [[k v] argmap]
                               [k (cond (= k :output-directory) v
                                        :else (read-string v))])))   
        random-seed (if (:random-seed argmap)
                      (byte-array (map byte (read-string (:random-seed argmap)))) 
                      (generate-random-seed))
        arg-params (merge @params argmap)
        rng (make-RNG random-seed)]    
    (println argmap)
    (println arg-params)
    (reset! params arg-params)
    (swap! params assoc :environment-type (:initial-environment-type @params))
    (print-params)
    
    (with-rng rng
      ((if (:gui @params) start-gui start-nogui) 
        initialize-simulation java-update-world))))

;; For autostart with Counterclockwise in Eclipse
(when (find-ns 'ccw.complete)
 (-main))
