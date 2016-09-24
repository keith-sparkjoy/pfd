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

(defn add-vasteps
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
      add-vasteps))

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

(defn current-step
  [{:keys [pfd sid]}]
  (get-in pfd [:steps sid]))

(defn va-sid?
  [pfd sid]
  (contains? (:vasids pfd) sid))

(defn text-for-current-step
  [{:keys [pfd sid] :as cursor}]
  (if (nil? sid)
    (:eboundary pfd)
    (let [step (current-step cursor)
          text (or (:directive step)
                   (:predicate step))]
      (if (va-sid? pfd sid)
        text
        (str "(NVA) " text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cursors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-cursor
  "Create a cursor to walk through a process flow diagram"
  [pfd]
  {:pfd pfd
   :sid (:beginning-sid pfd)})

(defn nav-cursor
  "Returns a function that will navigate the cursor to the next step"
  [type-predicate get-next-sid]
  (fn [cursor]
    (let [step (current-step cursor)
          new-cursor (when (type-predicate step)
                       (let [next-sid (get-next-sid step)]
                         (update-in cursor [:sid] (fn [old-sid] next-sid))))]
      (or new-cursor cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL experimentation stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-sample-pfd
  []
  (let [pfd {:name "OpenFrontDoor"
             :aim "Open front door"
             :bboundary "You are standing outside the door."
             :eboundary "The door is open and you are standing inside."
             :steps [{:sid 5
                      :directive "Turn the door knob."
                      :next 3}
                     {:sid 3
                      :predicate "Is the door locked?"
                      :not true
                      :consequent 42
                      :alternate 9}
                     {:sid 42
                      :directive "Gently push the door open and step inside."}
                     {:sid 9
                      :directive "Retrieve house key from pocket"
                      :next 67}
                     {:sid 67
                      :directive "Insert key and twist counterclockwise."
                      :next 5}]
             :beginning-sid 5}]
    (add-computed-fields pfd)))

(def tpfd (atom (make-sample-pfd)))
(def cursor (atom nil))

(defn run []
  (reset! cursor (new-cursor @tpfd))
  (let [{:keys [bboundary]} @tpfd]
    (str bboundary " " (text-for-current-step @cursor))))

(defn ok []
  (swap! cursor (nav-cursor is-directive? :next))
  (text-for-current-step @cursor))

(defn y []
  (letfn [(get-next-sid
            [{:keys [not consequent alternate]}]
            (if not alternate consequent))]
    (swap! cursor (nav-cursor is-predicate? get-next-sid))
    (text-for-current-step @cursor)))

(defn n []
  (letfn [(get-next-sid
            [{:keys [not consequent alternate]}]
            (if not consequent alternate))]
    (swap! cursor (nav-cursor is-predicate? get-next-sid))
    (text-for-current-step @cursor)))
