(ns pfd.repl-nav
  (:require [pfd.core :refer :all]))

(defn make-sample-pfd
  []
  (let [pfd {:name "open-front-door"
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
                      :next 55}
                     {:sid 55
                      :predicate "Do you have the key?"
                      :consequent 67}
                     {:sid 67
                      :directive "Insert key and twist counterclockwise."
                      :next 5}]
             :beginning-sid 5}]
    (add-computed-fields pfd)))

(def tpfd (atom (make-sample-pfd)))
(def cursor (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL nav cursor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-cursor
  "Create a cursor to walk through a process flow diagram"
  ([pfd]
   {:pfd pfd
    :sid (:beginning-sid pfd)})
  ([pfd sid most-recent-sid]
   {:pfd pfd
    :sid sid
    :mrsid most-recent-sid}))

(defn current-step
  [{:keys [pfd sid]}]
  (get-in pfd [:steps sid]))

(defn end?
  [{:keys [sid]}]
  (nil? sid))

(defn ended-successfully?
  [{:keys [pfd mrsid] :as cursor}]
  (assert (end? cursor))
  (va-sid? pfd mrsid))

(defn nav-cursor
  "Returns a function that will navigate the cursor to the next step"
  [expected-type? get-next-sid]
  (fn [cursor]
    (let [step (current-step cursor)
          new-cursor (when (expected-type? step)
                       (let [most-recent-sid (:sid step)
                             pfd (:pfd cursor)
                             next-sid (get-next-sid step)]
                         (new-cursor pfd next-sid most-recent-sid)))]
      (or new-cursor cursor))))

(defn text-for-current-step
  [{:keys [pfd sid] :as cursor}]
  (if (end? cursor)
    (if (ended-successfully? cursor)
      (str "Success! " (:eboundary pfd))
      "Failure :-(")
    (let [step (current-step cursor)
          text (or (:directive step)
                   (:predicate step))]
      (if (va-sid? pfd sid)
        text
        (str "(NVA) " text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REPL nav commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run []
  (reset! cursor (new-cursor @tpfd))
  (let [{:keys [bboundary]} @tpfd]
    (str bboundary " " (text-for-current-step @cursor))))

(defn ok []
  (swap! cursor (nav-cursor is-directive? :next))
  (text-for-current-step @cursor))

(defn answer
  [y?]
  (swap! cursor (nav-cursor is-predicate? (get-next-sid-for-predicate y?)))
  (text-for-current-step @cursor))

(defn y []
  (answer true))

(defn n []
  (answer false))
