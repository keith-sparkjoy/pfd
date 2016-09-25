(ns pfd.step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Step
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PfdStep

  (value-add? [this] "Is this a value-added step?")

  (text [this] "Text for this step (either directive or predicate)"))

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
  "Constructs a concrete implementation of PfdStep given a raw step from a PFD and caches the results of analysis to determine if this step was in the value-add line (typically represented as the main vertical flow) or non-value-add (typically represented horizontally - off to the milky way). Non-value-add is often abbreviated NVA."
  [m value-add]
  (let [map-ctor (cond (contains? m :directive) map->PfdDirective
                       (contains? m :predicate) map->PfdPredicate)]
    (map-ctor {:m m
               :value-add value-add})))
