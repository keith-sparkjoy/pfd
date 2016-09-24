(ns pfd.core)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; process flow diagram (PFD)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-steps
  "Replaces a seq of steps with a map making lookup easier"
  [{:keys [steps] :as pfd}]
  (update-in pfd
             [:steps]
             (fn [steps]
               (zipmap (map :sid steps) steps))))

(defn beginning-step
  "Look up the first step for this PFD"
  [pfd]
  ((:steps pfd) (:beginning-sid pfd)))

(defn add-vasids
  "All value-added steps for a process can be found by traversing steps from the beginning through all directives and consequents. This function computes them ahead of time and adds them to the PFD."
  [{:keys [steps] :as pfd}]
  (letfn [(next-va-step [step]
            (steps (or (:next step)
                       (:consequent step))))
          (collect-va-sids [sids step]
            (if (nil? step)
              sids
              (recur (conj sids (:sid step)) (next-va-step step))))]
    (conj pfd {:vasids (collect-va-sids #{} (beginning-step pfd))})))

(defn add-computed-fields
  "Designed for use after retrieving a PFD from persistant storage. QUESTION - should these computed fields be added to the PFD or the cursor that traverses it?"
  [pfd]
  (-> pfd
      map-steps
      add-vasids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; steps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-directive?
  [step]
  (contains? step :directive))

(defn is-predicate?
  [step]
  (contains? step :predicate))

(defn va-sid?
  "Does this SID identify a value-added step?"
  [pfd sid]
  (contains? (:vasids pfd) sid))

(defn get-next-sid-for-predicate
  "Given an answer, returns a function that will find the next step from a given predicate step"
  [y?]
  (fn [step]
    ((if y? first second)
     ((if (:not step) reverse identity)
      (vals (select-keys step [:consequent :alternate]))))))
