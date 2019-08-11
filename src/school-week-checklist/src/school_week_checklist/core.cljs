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

(defn parse-date
  [s]
  (js/Date. (str s " 12:00"))) ; so UTC string will still be right date in USA.

(defn begining-of-week
  [dt]
  (doto (.addDays dt (- (.getDay dt)))
    (.setHours 0)
    (.setMinutes 0)
    (.setSeconds 0)
    (.setMilliseconds 0)))

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
                                   :out vec}
                   :course-order {:in #(into [] %)}
                                 :default #js []
                                 :out vec})
(def state-keys (keys state-xforms))

(def default-csv "Date,Subject,Course,Lesson,Time,Points,Possible,Complete\n2018-09-10,Mathematics,Singapore Math 2B/3A,\"TB pp. 95-96, WB pp. 146-148\",30,,,\n2018-09-10,Language Arts,Spelling Power,Daily Words/Drills or Writing,15,,,\n2018-09-10,Language Arts,Language Lessons... L4,Lesson 16,15,,,\n2018-09-10,Reading,Read Aloud,Mom's Choice,30,,,\n2018-09-10,Reading,Silent Reading Time,Book of Choice,30,,,\n2018-09-10,Physical Education,Physical Education,Go Noodle/Outside,30,,,\n2018-09-10,Classical Conversations,CC Presentations,Perform Presentation,15,,,\n2018-09-10,Classical Conversations,CC Memory Work,W2,30,,,\n2018-09-10,Classical Conversations,Daily Focus,Math,60,,,\n2018-09-10,Mathematics,Game Time,Math Game,30,,,\n2018-09-10,Life Skills,Morning Routine/Chores,Daily Chores,30,,,\n2018-09-10,Language Arts,Handwriting,pp. 40-41,15,,,\n2018-09-10,Language Arts,Typing/Spanish,Spanish Lesson,30,,,\n2018-09-10,Fine Arts,Piano Lessons,Practice Piano,15,,,\n2018-09-11,Classical Conversations,CC Science,Some parts of an animal cell,,,,\n2018-09-11,Classical Conversations,CC Math,5s and 6s,,,,\n2018-09-11,Classical Conversations,CC Art/Music,Upside-Down Image,,,,\n2018-09-11,Physical Education,Physical Education,Go Noodle/Outside,30,,,\n2018-09-11,Classical Conversations,CC History,Greek and Roman gods,,,,\n2018-09-11,Classical Conversations,CC Geography,Hebrew Empire,,,,\n2018-09-11,Classical Conversations,CC Latin,1st Declension Noun Endings,,,,\n2018-09-11,Classical Conversations,CC English,Prepositions along-atop,,,,\n2018-09-11,Classical Conversations,Daily Focus,Community Day Week 3,180,,,\n2018-09-11,Life Skills,Morning Routine/Chores,Daily Chores,30,,,\n2018-09-11,Classical Conversations,CC Timeline,Hinduism in India-Israel's United Kingdom,,,,\n2018-09-12,Mathematics,Singapore Math 2B/3A,\"TB pp. 97-99, WB pp. 149-151\",30,,,\n2018-09-12,Language Arts,Spelling Power,Daily Words/Dictionary,15,,,\n2018-09-12,Language Arts,Language Lessons... L4,Lesson 17,15,,,\n2018-09-12,Reading,Read Aloud,Mom's Choice,30,,,\n2018-09-12,Reading,Silent Reading Time,Book of Choice,30,,,\n2018-09-12,Physical Education,Physical Education,Go Noodle/Outside,30,,,\n2018-09-12,Classical Conversations,CC Presentations,Pick Presentation,15,,,\n2018-09-12,Classical Conversations,CC Memory Work,\"W3, W2\",30,,,\n2018-09-12,Classical Conversations,Daily Focus,History,60,,,\n2018-09-12,Mathematics,Game Time,Kid Pick,30,,,\n2018-09-12,Life Skills,Morning Routine/Chores,Daily Chores,30,,,\n2018-09-12,Language Arts,Handwriting,pp. 42-43,15,,,\n2018-09-12,Language Arts,Typing/Spanish,Typing Lesson,30,,,\n2018-09-12,Fine Arts,Piano Lessons,Piano Lessons,30,,,\n2018-09-13,Mathematics,Singapore Math 2B/3A,TB pp. 100-101 (Review H),30,,,\n2018-09-13,Language Arts,Spelling Power,Daily Words/Homonymns,15,,,\n2018-09-13,Language Arts,Language Lessons... L4,Lesson 18,15,,,\n2018-09-13,Reading,Read Aloud,Mom's Choice,30,,,\n2018-09-13,Reading,Silent Reading Time,Book of Choice,30,,,\n2018-09-13,Physical Education,Physical Education,Go Noodle/Outside,30,,,\n2018-09-13,Classical Conversations,CC Presentations,Practice Presentation,15,,,\n2018-09-13,Classical Conversations,CC Memory Work,\"W3, W1\",30,,,\n2018-09-13,Classical Conversations,Daily Focus,Science,60,,,\n2018-09-13,Mathematics,Game Time,Flashcard Drills,30,,,\n2018-09-13,Life Skills,Morning Routine/Chores,Daily Chores,30,,,\n2018-09-13,Language Arts,Handwriting,pp. 44-47,15,,,\n2018-09-13,Language Arts,Handwriting,Parent Needed,,,,\n2018-09-13,Language Arts,Typing/Spanish,Spanish Lesson,30,,,\n2018-09-13,Fine Arts,Piano Lessons,Practice Piano,15,,,\n2018-09-14,Mathematics,Singapore Math 2B/3A,WB pp. 152-155 (Review 6),30,,,\n2018-09-14,Language Arts,Spelling Power,Daily Words/Skill Build,15,,,\n2018-09-14,Language Arts,Language Lessons... L4,Lesson 19,15,,,\n2018-09-14,Reading,Read Aloud,Mom's Choice,30,,,\n2018-09-14,Reading,Silent Reading Time,Book of Choice,30,,,\n2018-09-14,Physical Education,Physical Education,Go Noodle/Outside,30,,,\n2018-09-14,Classical Conversations,CC Presentations,Practice Presentation,15,,,\n2018-09-14,Classical Conversations,CC Memory Work,\"W3, W2\",30,,,\n2018-09-14,Classical Conversations,Daily Focus,English,60,,,\n2018-09-14,Life Skills,Morning Routine/Chores,Daily Chores,30,,,\n2018-09-14,Language Arts,Handwriting,pp. 48-52,15,,,\n2018-09-14,Language Arts,Handwriting,Parent Needed,,,,\n2018-09-14,Language Arts,Typing/Spanish,Typing Lesson,30,,,\n2018-09-14,Fine Arts,Piano Lessons,Practice Piano,15,,,\n2018-09-14,Fine Arts,Art Lessons,Art Lessons,30,,,")

(def default-state
  {:csv default-csv
   :group-key :course
   :min-date nil})

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

(defn pretty-date
  [dt]
  (string/replace (str dt) #"^\w+\s|\s\d+:.*" ""))


(defn parse-csv-maps
  [csv]
  (let [[header & rows] (csv/parse csv)
        row-data (map #(zipmap (map (fn [k](-> k string/lower-case keyword)) header)
                               %)
                      rows)]
    (map (fn add-parsed-date [{:keys [date] :as entry}]
           (assoc entry :js-date (parse-date date)))
      row-data)))

(defn default-min-date
  [entries]
  (let [week-dates (into (sorted-set) (map (comp begining-of-week :js-date) entries))
        this-week (begining-of-week (js/Date.))
        mx-date (first week-dates)
        mn-date (last week-dates)]
    (cond (nil? mx-date)         this-week
          (>= this-week mx-date) mx-date
          (>= mn-date this-week) mn-date
          :default               this-week)))

(defn incorporate-csv
  [state csv]
  (let [entries (parse-csv-maps csv)
        new-state
         (assoc state
          :csv csv
          :entries entries
          :courses (into (sorted-set) (map :course entries)))]
      (update new-state :course-order
        (fn [course-order]
          (reduce
            (fn [order course]
              (cond-> order
                (neg? (.indexOf order course)) (conj course)))
            course-order
            (get new-state :courses))))))

(defn show-example!
  []
  (swap! app-state incorporate-csv default-csv))

(defn file-changed
  []
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

(defn course-order-changed
  [course-order {:keys [course relative-position]}]
  (println course relative-position)
  (let [idx (.indexOf course-order course)
        before (subvec course-order 0 idx)
        after (subvec course-order (inc idx))
        without (reduce into [] [before after])
        new-index (when relative-position
                     (-> (+ idx relative-position)
                         (max 0)
                         (min (count without))))
        new-order
        (cond-> without
          new-index (-> (subvec 0 new-index)
                        (conj course)
                        (into (subvec without new-index))))]
    new-order))

(defn day-of-week
  [s]
  (try
    (-> (parse-date s)
        (str)
        (string/replace #" .*" ""))
    (catch :any ex
      nil)))

(defn clean-lesson
  [s]
  (string/replace s "-" "‑"))

(defn re-order
  [items preferred-order]
  (let [preferred (reduce (fn [m [i item]](assoc m item i)) {} (map-indexed vector preferred-order))]
    (sort-by #(get preferred % (count preferred-order)) items)))

(defn calendar-table
  [{:keys [group-key entries hidden-groups course-order min-date max-date] :as state}]
  (let [grouped-entries (group-by (juxt group-key :date) entries)
        row-keys (into (sorted-set) (map ffirst grouped-entries))
        col-keys (filter
                    #(<= (.valueOf min-date)
                         (.valueOf (parse-date %))
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
         (for [row-key (re-order (remove hidden-groups row-keys) course-order)]
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
  [{:keys [courses hidden-groups course-order] :as state}]
  [:div.no-print {:key "courses"}
    [:ol
     (for [[idx course] (map-indexed vector (re-order courses course-order))
           :let [hidden (contains? hidden-groups course)]]

       [:li {:key (str course)}
        [:label {:class (str #_"checkbox " (when hidden "disabled")), :for course}
         [:input {:key "i", :id course, :name course
                  :type "checkbox"
                  :class "custom-checkbox"
                  :value course
                  :checked (if hidden "" "checked")
                  :onChange #(swap! app-state update :hidden-groups group-visibility-changed {course hidden})}]
         course]
        (when (pos? idx)
          [:button {:onClick  #(swap! app-state update :course-order course-order-changed {:course course :relative-position -1})} "Move up"])
        (when (< (inc idx) (count course-order))
          [:button {:onClick  #(swap! app-state update :course-order course-order-changed {:course course :relative-position  1})} "Move down"])
        [:button {:onClick  #(swap! app-state update :course-order course-order-changed {:course course :relative-position nil})} "Move to bottom"]])]])

(defn date-selector
  [{:keys [min-date entries] :as state}]
  (let [min-date-date (.toLocaleDateString min-date)
        cur-date-date (.toLocaleDateString (js/Date.))]
    [:div.no-print {:key "date-selector"}
      (for [week-start (->> (keep :date entries)
                            (map parse-date)
                            (keep begining-of-week)
                            distinct)]
         [:label {:key (str week-start)
                  :name (str week-start)
                  :class (str "checktext "
                               (when (= cur-date-date (.toLocaleDateString week-start)) "current ")
                               "week-selector ")}
          [:input (cond-> {:type "radio"
                           :onClick #(swap! app-state assoc :min-date week-start)
                           :checked (= min-date-date (.toLocaleDateString week-start))})]
          [:span
           (pretty-date week-start)]])]))

(defn app-component
  []
  (let [{:keys [min-date entries] :as state} @app-state
        min-date (or min-date (default-min-date entries))
        state (assoc state :min-date min-date)]
    [:div
     [:div.no-print
       [:h1 "School Week Checklist"]
       [:div#about "This web sites has no affiliation with Scholaric, but my family uses their great software.  This makes uses of the data stored in Scholaric.  You're welcome to use it too."]
       [:h3 "1. Export your Scholaric schedule"]
       [:ol
        [:li "Launch "
         [:a {:target "_new"
              :href "https://scholaric.com/csv_exports"}
          "Scholaric"]
         ", then select the student and All Subjects.  It will then email your export."]
        [:li "Check email for the export and Save As.  "]]
       [:h3 "2. Next, open that file"]
       [:ul
        [:li
         "Open the schedule"
         [:br]
         [:input#csv-file
            {:type "file", :accept ".csv", :name "csv"
             :onChange #(file-changed)}]
         [:br]
         [:span "This file stays on your computer.  It isn't uploaded anywhere."]]
        [:li
         "Or, you can see an example "
         [:br]
         [:button {:onClick show-example!}
          "Load Example"]]]
       [:h3 "3. What week do you want to print?"]
       [:div (date-selector state)]
       [:h3 "4. Preview Checklist"]
       [:ul
        [:li "Enter Student's name, and any special days and comments."]
        [:li "You can select which classes you would like to see below the preview."]]]
     [:div.preview
       [:h3.header
        [:input#student.no-print-border {:placeholder "Student"}]
        [:span "Week of " (pretty-date min-date)]]
       [:hr.no-print]
       (calendar-table
          (assoc state :max-date (.addDays min-date 7)))
       [:h5.footer
        "Rendered "
        (.toLocaleString (js/Date.))]
       [:input#message.no-print-border {:placeholder "Thanks for being a good student!"}]]
     [:div.no-print
        [:h3 "5. Which courses do you want to include?"]
        (course-selector state)
        [:h3 "6. Are you ready to print?"
         [:br]]
        [:ul
         [:a {:href "javascript:window.print()"}
          [:button "Print"]]
         [:br]
         [:b "Note: "]
         [:span "You may wish to turn off Headers and Footers for a cleaner copy."]]]]))


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
