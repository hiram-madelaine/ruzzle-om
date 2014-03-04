(ns ruzzle-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! <!]]
            [clojure.string :as string]))

(enable-console-print!)


(def app-state (atom {:board [{:letter :A, :id 0} {:letter :B, :id 1} {:letter :E, :id 2} {:letter :Y, :id 3} {:letter :P, :id 4} {:letter :I, :id 5} {:letter :D, :id 6} {:letter :C, :id 7} {:letter :L, :id 8} {:letter :A, :id 9} {:letter :N, :id 10} {:letter :N, :id 11} {:letter :E, :id 12} {:letter :R, :id 13} {:letter :R, :id 14} {:letter :E, :id 15}]}))

(def letter-values {:P 2 :R 2 :Q 4 :Y 4 :Z 4 })

(def multiples {0 3})

(defn id->letter [owner id]
  (let [{:keys [board]} (om/get-props owner)]
    (first (filter #(= id (:id %)) board))))


(defmulti letter-event (fn [[e _] owner] e))


(defmethod letter-event :hover
  [[_ id] owner]
  (when-let [capture (om/get-state owner [:clicked])]
    (let [captured (om/get-state owner [:captured])]
      (when-not (some #{id} captured)
        (om/update-state! owner [:captured] #(conj % id))))))


(defmethod letter-event :click
  [[_ id] owner]
  (let [{:keys [clicked ]} (om/get-state owner)]
        (om/update-state! owner [:captured] #(conj % id))
    (when clicked
      (om/set-state! owner [:captured] []))
    (om/update-state! owner [:clicked] not)))



(defn letter-view
  "Handle the display of a single letter"
  [{:keys [letter id] :as app} owner]
  (reify
    om/IRenderState
    (render-state
     [_ {:as state :keys [chan captured]}]
     (let [{:keys [values multiples]} (om/get-shared owner)
           styles ["letter"]
           styles (if (some #{id} captured) (conj styles "selected") styles )]
       (dom/div #js {:className (string/join " " styles)
                     :onClick #(put! chan [:click id])}
                (dom/div #js {:className "holder"}
                         (dom/div #js {:className "valeur"} (get values letter 1))
                         (dom/h1 #js {:onMouseOver  #(put! chan [:hover id])} (name letter))))))))

(defn board-view
  "Display the 4x4 letters board."
  [app owner]
  (reify
    om/IRenderState
    (render-state
     [_ {:as state :keys [clicked captured]}]
     (apply dom/div #js {:className "board"}
              (om/build-all letter-view (:board app)
                            {:key :id
                             :state state})))))

(defn selected-view
  "Selected letter view."
  [letter owner]
  (om/component
   (dom/span nil (name (:letter letter)))))

(defn selection-view
  "Dsplays the current selected letters."
  [app owner]
  (reify
    om/IRenderState
    (render-state [_ {:as state :keys [chan captured]}]
                 (apply dom/span nil
                        (om/build-all selected-view (:captured state) {:fn (partial id->letter owner)})))))

(defn app-view [app owner]
  (reify
   om/IInitState
    (init-state [_]
                {:chan (chan)
                 :clicked false
                 :captured []})
    om/IWillMount
    (will-mount [_]
       (let [chan (om/get-state owner [:chan])]
         (go (loop []
               (let [r (<! chan)]
                 (letter-event r owner)
                 (recur))))))
    om/IRenderState
    (render-state [_ state]
                  (dom/div nil
                           (om/build board-view app {:state state})
                           (om/build selection-view app {:state state})))))

(om/root
 app-view
  app-state
  {:target (. js/document (getElementById "app"))
   :shared {:values letter-values
            :multiples multiples}})
