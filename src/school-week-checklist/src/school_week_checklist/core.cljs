(ns school-week-checklist.core
    (:require [clojure.string :as string]
              [reagent.core :as r]
              [goog.labs.format.csv :as csv]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce _add-days
  (set! (.-addDays (.-prototype js/Date))
     (fn add-days [days]
       (this-as this
          (let [date (js/Date. (.valueOf this))]
            (.setDate date (+ (.getDate date) days))
            date)))))

(defn begining-of-week
  [dt]
  (.addDays dt (- (.getDay dt))))

(defn local-storage
  [state-key js-default xform-from-clj]
  (cond-> (js->clj
            (if-let [s (.getItem js/localStorage (name state-key))]
              (.parse js/JSON s)
              js-default))
    xform-from-clj xform-from-clj))

(defn local-storage!
  [state-key value xform-to-clj]
  (cond->> value
    xform-to-clj xform-to-clj
    true         clj->js
    true         (.stringify js/JSON)
    true         (.setItem js/localStorage (name state-key))))

(def state-xforms {:hidden-groups {:in #(into (sorted-set) %)
                                   :default #js []
                                   :out vec}})
(def state-keys (keys state-xforms))

(def default-csv "key,value\r\n1,a\r\n2,b")

(def default-state
  {:csv default-csv
   :group-key :course
   :min-date (begining-of-week (js/Date. "9/9/2018"))
   :tick 0})

(defonce app-state
  (r/atom (reduce-kv
            (fn hydrate-local [st k {xform :in, default :default}]
              (assoc st k (local-storage k default xform)))
            default-state
            state-xforms)))

(defonce _state-saver
  (add-watch app-state :state-saver
    (fn state-saver [k r old new]
      (let [old-st (select-keys old state-keys)
            new-st (select-keys new state-keys)]
        (doseq [k state-keys
                :let [ov (get old-st k)
                      nv (get new-st k)
                      xform (get-in state-xforms [k :out])]]
          (when (not= ov nv)
            (local-storage! k nv xform)))))))

(defonce _ticker
  (js/setInterval #(swap! app-state update :tick inc) 10000))

(defn pretty-date
  [dt]
  (string/replace (str dt) #"^\w+\s|\s\d+:.*" ""))


(defn parse-csv-maps
  [csv]
  (let [[header & rows] (csv/parse csv)
        row-data (map #(zipmap (map (fn [k](-> k string/lower-case keyword)) header)
                               %)
                      rows)]
    row-data))

(defn incorporate-csv
  [state csv]
  (let [entries (parse-csv-maps csv)]
    (assoc state
      :csv csv
      :entries entries
      :courses (into (sorted-set) (map :course entries)))))


(defn file-changed
  [event]
  (let [rdr (js/FileReader.)]
    (set! (.-onload rdr) #(let [csv (.-result rdr)]
                            (swap! app-state incorporate-csv csv)))

    (.readAsText rdr (-> (.getElementById js/document "csv-file") .-files (aget 0)))))

(defn group-visibility-changed
  [hidden-groups m]
  (reduce-kv
    (fn [s k v]
      ((if v disj conj) s k))
    hidden-groups
    m))

(defn day-of-week
  [s]
  (try
    (-> (js/Date. (str s " 12:00")) ; so UTC string will still be right date in USA.
        (str)
        (string/replace #" .*" ""))
    (catch :any ex
      nil)))

(defn clean-lesson
  [s]
  (string/replace s "-" "‑"))

(defn calendar-table
  [{:keys [group-key entries hidden-groups min-date max-date] :as state}]
  (let [grouped-entries (group-by (juxt group-key :date) entries)
        row-keys (into (sorted-set) (map ffirst grouped-entries))
        col-keys (filter
                    #(<= (.valueOf min-date)
                         (.valueOf (js/Date. (str % " 12:00")))
                         (.valueOf max-date))
                    (into (sorted-set) (map (comp second first) grouped-entries)))]
    [:div.calendar-table
      [:table.calendar-table
        [:thead
         [:tr
          [:td "Class"]
          (for [date-string col-keys]
            [:td {:key date-string} (day-of-week date-string) " " (name date-string)
             [:input.no-print-border {:placeholder "about"}]])]]
        [:tbody
         (for [row-key (remove hidden-groups row-keys)]
           [:tr {:key row-key}
            [:td {:key "h"} (name row-key)]
            (for [col-key col-keys]
              [:td {:key col-key}
               (for [[idx {:keys [lesson] :as entry}] (map-indexed vector (get grouped-entries [row-key col-key]))]
                 [:span.check {:key (str idx)}
                  (clean-lesson lesson)])])])]]
      #_[:div.flow
         (let [row-keys (filter hidden-groups row-keys)
               entries (for [row-key row-keys
                             col-key col-keys]
                         (get grouped-entries [row-key col-key]))
               lessons (->> (flatten entries)
                            (keep (fn footer-lesson [entry]
                                    (when entry
                                     (str (get entry :lesson)
                                          " – "
                                          (get entry group-key)))))
                            (map-indexed vector))]
           (for [[idx lesson] lessons]
             [:span.check {:key (str idx)}
              (clean-lesson lesson)]))]]))

(defn course-selector
  [{:keys [courses hidden-groups] :as state}]
  [:ol.no-print {:key "courses"}
    [:h5 "Courses"
     (for [course courses
           :let [hidden (contains? hidden-groups course)]]

       [:li {:key (str course)}
         [:input {:key "i", :id course
                  :type "checkbox"
                  :value course
                  :checked (if hidden "" "checked")
                  :onChange #(swap! app-state update :hidden-groups group-visibility-changed {course hidden})}]
         [:label {:class (when hidden "disabled"), :for course}
          course]])]])

(defn date-selector
  [{:keys [min-date entries] :as state}]
  (let [min-date-date (.toLocaleDateString min-date)
        cur-date-date (.toLocaleDateString (js/Date.))]
    [:div.no-print {:key "date-selector"}
      (for [week-start (->> (keep :date entries)
                            (map #(js/Date. (str % " 12:00")))
                            (keep begining-of-week)
                            distinct)]
         [:button {:key (str week-start)
                   :onClick #(swap! app-state assoc :min-date week-start)
                   :class (str (when (= min-date-date (.toLocaleDateString week-start)) "active ")
                               (when (= cur-date-date (.toLocaleDateString week-start)) "current "))}
           (pretty-date week-start)])]))

(defn app-component
  []
  (let [{:keys [min-date] :as state} @app-state]
    [:div
     [:div.no-print
       [:h1 "School Week Checklist"]
       [:h3 "1. Export your Scholaric schedule"]
       [:a {:target "_new"
            :href "https://scholaric.com/csv_exports"}]
       [:h3 "2. Check email for the export and Save As.  Next, choose that file here:"]
       [:input#csv-file
          {:type "file", :accept ".csv", :name "csv"
           :onChange #(file-changed js/event)}]
       [:span "This file stays on your computer.  It isn't uploaded anywhere."]
       [:h3 "3. What week do you want to print?"]
       [:div (date-selector state)]
       [:h3 "4. Enter Student's name, and any special days and comments."]]
     [:h1
      [:input#student.no-print-border {:placeholder "Student"}]
      [:span "Week of " (pretty-date min-date)]]
     [:hr.no-print]
     (calendar-table
        (assoc state :max-date (.addDays min-date 7)))
     [:h5.footer
      "Rendered "
      (.toLocaleString (js/Date.))]
     [:input#message.no-print-border {:placeholder "Thanks for being a good student!"}]
     [:div.no-print
        [:h3 "5. Which courses do you want to include?"]
        (course-selector state)
        [:h3 "6. Are you ready to print?"
         [:br]
         [:a {:href "javascript:window.print()"}
          [:button "Print"]]]
        [:br]
        [:b "Note:"]
        [:span "You may wish to turn off Headers and Footers for a cleaner copy."]]]))


(def app-elem (atom nil))

(defn ^:export mount-app
  [elem]
  (reset! app-elem elem)
  (r/render [app-component]
            (or elem @app-elem (.-body js/document))))

(defn on-js-reload
  []
  (swap! app-state update-in [:__figwheel_counter] inc)
  (swap! app-state #(incorporate-csv % (:csv %)))
  (mount-app @app-elem))
