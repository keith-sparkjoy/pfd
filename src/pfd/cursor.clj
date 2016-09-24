(ns pfd.cursor)

(declare nav-cursor
         get-next-step-for-directive
         get-next-step-for-predicate)

(defprotocol PfdNavigator

  (end? [this])
  (ended-successfully? [this])
  (current-step [this])

  (ok [this])
  (y [this])
  (n [this]))

(defrecord PfdCursor [pfd stepmap vasids curstep mrstep]

  PfdNavigator

  (current-step
    [{:keys [curstep]}]
    curstep)

  (end?
    [{:keys [curstep]}]
    (nil? curstep))

  (ended-successfully?
    [{:keys [mrstep vasids]}]
    (contains? vasids (:sid mrstep)))

  (ok [this]
    (nav-cursor this :directive get-next-step-for-directive))

  (y [this]
    (nav-cursor this :predicate (get-next-step-for-predicate true)))

  (n [this]
    (nav-cursor this :predicate (get-next-step-for-predicate false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Constructor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pfdcursor
  "Creates a new PdfCursor pointing at the beginning step."
  [{:keys [steps beginning-sid] :as pfd}]
  (let [stepmap (zipmap (map :sid steps) steps)
        beginning-step (stepmap beginning-sid)]
    (letfn [(next-va-step [step]
              (stepmap (or (:next step)
                           (:consequent step))))
            (collect-va-sids [sids step]
              (if (nil? step)
                sids
                (recur (conj sids (:sid step)) (next-va-step step))))]
      (map->PfdCursor {:pfd pfd
                       :stepmap stepmap
                       :vasids (collect-va-sids #{} beginning-step)
                       :curstep beginning-step}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Private Helpers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- nav-cursor
  "Returns a new cursor navigated to the next step"
  [{:keys [stepmap curstep] :as old-cursor} expected-key get-next-step]
  (let [new-cursor (when (contains? curstep expected-key)
                     (assoc old-cursor
                            :mrstep curstep
                            :curstep (get-next-step stepmap curstep)))]
    (or new-cursor old-cursor)))

(defn- get-next-step-for-directive
  [stepmap curstep]
  (stepmap (:next curstep)))

(defn- get-next-step-for-predicate
  "Given a Y/N answer, returns a function that will find the next step from a given predicate step"
  [y?]
  (fn [stepmap curstep]
    (let [reverse-if-negative (if (:not curstep) reverse identity)
          possibilities (reverse-if-negative
                         (vals
                          (select-keys curstep [:consequent :alternate])))
          select (if y? first second)
          next-sid (select possibilities)]
      (stepmap next-sid))))
