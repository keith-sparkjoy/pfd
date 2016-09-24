(ns user
  (:require [pfd.cursor :as c]))

(defn sample-pfd
  []
  {:name "open-front-door"
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
   :beginning-sid 5})

(def cursor (atom nil))

(defn va-step?
  [{:keys [vasids curstep]}]
  (contains? vasids (:sid curstep)))

(defn text-for-current-step
  [{:keys [pfd curstep] :as cursor}]
  (if (c/end? cursor)
    (if (c/ended-successfully? cursor)
      (str "Success! " (:eboundary pfd))
      "Failure :-(")
    (let [step (c/current-step cursor)
          text (or (:directive step)
                   (:predicate step))]
      (if (va-step? cursor)
        text
        (str "(NVA) " text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use these functions to nav the sample
;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn begin []
  (reset! cursor (c/pfdcursor (sample-pfd)))
  (let [pfd (:pfd @cursor)
        bboundary (:bboundary pfd)]
    (str bboundary " " (text-for-current-step @cursor))))

(defn ok []
  (swap! cursor c/ok)
  (text-for-current-step @cursor))

(defn y []
  (swap! cursor c/y)
  (text-for-current-step @cursor))

(defn n []
  (swap! cursor c/n)
  (text-for-current-step @cursor))


