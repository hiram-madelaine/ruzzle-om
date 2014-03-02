(ns ruzzle-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! <!]]))

(enable-console-print!)


(def app-state (atom {:selected []
                      :board [{:letter :A, :id 0} {:letter :B, :id 1} {:letter :E, :id 2} {:letter :Y, :id 3} {:letter :P, :id 4} {:letter :I, :id 5} {:letter :D, :id 6} {:letter :C, :id 7} {:letter :L, :id 8} {:letter :A, :id 9} {:letter :N, :id 10} {:letter :N, :id 11} {:letter :E, :id 12} {:letter :R, :id 13} {:letter :R, :id 14} {:letter :E, :id 15}]}))

(def letter-values {:P 2 :R 2 :Q 4 :Y 4 :Z 4 })

(defn id->letter [owner id]
  (let [{:keys [board]} (om/get-props owner)]
    (first (filter #(= id (:id %)) board))))



(defmulti letter-event (fn [[e _] app owner] e))

(defn selected?
  "Is the id of a letter already selected ?"
  [owner id]
  (some #{id} (:selected (deref (om/get-props owner)))))

(defn select-letter!
  [app owner id]
  (when-not (selected? owner id)
    (om/transact! app #(update-in % [:selected] conj id))))


(defmethod letter-event :hover
  [[_ id] app owner]
  (when-let [capture (om/get-state owner [:clicked])]
    (select-letter! app owner id)))


(defmethod letter-event :click
  [[_ id] app owner]
  (let [{:keys [clicked]} (om/get-state owner)]
    (select-letter! app owner id)
    (when clicked (om/update! app :selected []))
    (om/set-state! owner [:clicked] (not clicked))))

(defn letter-view
  [{:keys [letter id] :as app} owner]
  (reify
    om/IRenderState
    (render-state
     [_ state]
     (let [values (om/get-shared owner :values)
          chan (om/get-state owner [:chan]) ]
       (dom/div #js {:className "letter"
                     :onClick #(put! chan [:click id])
                     :onMouseOver  #(put! chan [:hover id])}
                (dom/div #js {:className "holder"}
                         (dom/div #js {:className "valeur"} (get values letter 1))
                         (dom/h1 nil (name letter))))))))

(defn board-view
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
                {:chan (chan)
                 :clicked false
                 :selected []})
    om/IWillMount
    (will-mount [_]
       (let [chan (om/get-state owner [:chan])]
         (go (loop []
               (let [r (<! chan)]
                 (letter-event r app owner)
                 (recur))))))
    om/IRenderState
    (render-state
     [_ state]
     (apply dom/div #js {:className "board"}
              (om/build-all letter-view (:board app)
                            {:key :id
                             :state state})))))

(defn selected-view
  "Selected letter view."
  [app owner]
  (om/component
   (dom/span nil app)))

(defn selection-view
  [app owner]
  (reify
    om/IRenderState
    (render-state [_ state]
                 (apply dom/span nil
                        (om/build-all letter-view (:selected app) {:fn (partial id->letter owner)})))))

(defn app-view [app owner]
  (reify
    om/IRenderState
    (render-state [_ state]
                  (dom/div nil (om/build board-view app)
                           (om/build selection-view app)))))

(om/root
 app-view
  app-state
  {:target (. js/document (getElementById "app"))
   :shared {:values letter-values}})
