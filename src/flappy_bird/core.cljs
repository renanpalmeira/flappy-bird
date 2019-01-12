(ns flappy-bird.core
  (:require
    [reagent.core :as r]
    [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
    [cljs.core.async.macros :refer [go-loop go]]))

;; -------------------------
;; State

(def horiz-vel -0.15)
(def gravity 0.05)
(def jump-vel 21)
(def start-y 312)
(def bottom-y 561)
(def flappy-x 212)
(def flappy-width 57)
(def flappy-height 41)
(def pillar-spacing 324)
(def pillar-gap 158)                                        ;; 158
(def pillar-width 86)

(def starting-state {:in-jump           nil
                     :timer-running     false
                     :jump-count        0
                     :initial-vel       0
                     :start-time        0
                     :flappy-start-time 0
                     :flappy-y          start-y
                     :pillar-list
                                        [{:start-time 0
                                          :pos-x      900
                                          :cur-x      900
                                          :gap-top    200}]})

(defn reset-state [_ cur-time]
  (-> starting-state
      (update-in [:pillar-list] (fn [pls] (map #(assoc % :start-time cur-time) pls)))
      (assoc
        :start-time cur-time
        :flappy-start-time cur-time
        :in-jump nil
        :timer-running true)))

(def flap-state (r/atom starting-state))

;; -------------------------
;; Utils

(defn px [n] (str n "px"))

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

;; -------------------------
;; Actions

(defn jump [{:keys [cur-time jump-count] :as state}]
  (-> state
      (assoc
        :in-jump true
        :jump-count (inc jump-count)
        :flappy-start-time cur-time
        :initial-vel jump-vel)))

(defn unjump []
  (if (pos? (:jump-count @flap-state))
    (js/setTimeout #(swap! flap-state assoc :in-jump false) 900)
    nil))

;; -------------------------
;; Views

(defn score [{:keys [cur-time start-time] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- cur-time start-time) horiz-vel) 544)
                                         pillar-spacing)))
                 4)]
    (assoc st :score (if (neg? score) 0 score))))

(defn in-pillar? [{:keys [cur-x]}]
  (and (>= (+ flappy-x flappy-width)
           cur-x)
       (< flappy-x (+ cur-x pillar-width))))

(defn pillar-gap? [{:keys [flappy-y]} {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision? [{:keys [pillar-list] :as state}]
  (if (some #(or (and (in-pillar? %)
                      (not (pillar-gap? state %)))
                 (bottom-collision? state)) pillar-list)
    (assoc state :timer-running false)
    state))

(defn pillar [index
              {:keys [cur-x upper-height lower-height]}]

  ^{:key index}
  [:div.pillars
   [:div.pillar.pillar-upper {:style {:left   (px cur-x)
                                      :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left   (px cur-x)
                                      :height lower-height}}]])


(defn curr-pillar-pos [cur-time {:keys [pos-x start-time]}]
  (translate pos-x horiz-vel (- cur-time start-time)))

(defn new-pillar [cur-time pos-x]
  {:start-time cur-time
   :pos-x      pos-x
   :cur-x      pos-x
   :gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn update-pillars [{:keys [pillar-list cur-time] :as st}]
  (let [pillars-with-pos (map #(assoc % :cur-x (curr-pillar-pos cur-time %)) pillar-list)
        pillars-in-world (sort-by
                           :cur-x
                           (filter #(> (:cur-x %) (- pillar-width)) pillars-with-pos))]
    (assoc st
      :pillar-list
      (if (< (count pillars-in-world) 3)
        (conj pillars-in-world
              (new-pillar
                cur-time
                (+ pillar-spacing
                   (:cur-x (last pillars-in-world)))))
        pillars-in-world))))

(defn sine-wave [st]
  (assoc st
    :flappy-y
    (+ start-y (* 30 (.sin js/Math (/ (:time-delta st) 300))))))

(defn update-flappy [{:keys [time-delta initial-vel flappy-y jump-count] :as st}]
  (if (pos? jump-count)
    (let [cur-vel (- initial-vel (* time-delta gravity))
          new-y (- flappy-y cur-vel)
          new-y (if (> new-y (- bottom-y flappy-height))
                  (- bottom-y flappy-height)
                  new-y)]

      (assoc st
        :flappy-y new-y))
    (sine-wave st)))

;; -------------------------
;; Animation

(defn time-update [timestamp state]
  (-> state
      (assoc
        :cur-time timestamp
        :time-delta (- timestamp (:flappy-start-time state)))
      update-flappy
      update-pillars
      collision?
      score))

(defn time-loop [time]
  (let [new-state (swap! flap-state (partial time-update time))]
    (when (:timer-running new-state)
      (go
        (<! (timeout 30))
        (.requestAnimationFrame js/window time-loop)))))

(defn start-game []
  (.requestAnimationFrame
    js/window
    (fn [time]
      (reset! flap-state (reset-state @flap-state time))
      (time-loop time))))

(defn board []

  [:div.board {:onMouseDown (fn [e]
                              (swap! flap-state jump)
                              (.preventDefault e))

               :onMouseUp   (fn [e]
                              (unjump)
                              (.preventDefault e))}

   [:h1.score (:score @flap-state)]
   [:div (map-indexed pillar (:pillar-list @flap-state))]

   (if-not (:timer-running @flap-state)
     [:a.start-button {:on-click #(start-game)}
      (if (< 1 (:jump-count @flap-state)) "TRY AGAIN" "LET'S GO")]
     [:span])

   [:div {:class (str "flappy " (case (:in-jump @flap-state)
                                  true "flappy-up"
                                  false "flappy-down"
                                  ""))
          :style {:top (px (:flappy-y @flap-state))}}]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [board] (.getElementById js/document "board-area")))

(defn init! []
  (mount-root))

(init!)