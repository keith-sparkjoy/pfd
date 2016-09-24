(ns pfd.step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Step
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PfdStep

  (value-add? [this])
  (text [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Directive
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord PfdDirective [m value-add]

  PfdStep

  (value-add?
    [{:keys [value-add]}]
    value-add)

  (text
    [{:keys [m]}]
    (:directive m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Predicate
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord PfdPredicate [m value-add]

  PfdStep

  (value-add?
    [{:keys [value-add]}]
    value-add)

    (text
    [{:keys [m]}]
    (:predicate m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constructor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pfdstep
  [m value-add]
  (let [map-ctor (cond (contains? m :directive) map->PfdDirective
                       (contains? m :predicate) map->PfdPredicate)]
    (map-ctor {:m m
               :value-add value-add})))
