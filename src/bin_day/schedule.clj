(ns bin-day.schedule
  (:require [clojure.java.io :as jio]
            [clojure.string :as string])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(defn split-fields [line]
  (clojure.string/split line #","))

(def date-format (DateTimeFormatter/ofPattern "MMMM ‘yy d"))

(defn parse-date [ym-field day-field]
  (let [day (subs day-field 0 (- (count day-field) 2))
        date (str ym-field " " day)]
    (LocalDate/parse date date-format)))

(defn parse-schedule-line [line]
  (let [[ym-field & date-fields] (split-fields line)]
    (map #(parse-date ym-field %) date-fields)))

(defn parse-schedule-stream [file-stream]
  (->> file-stream
       line-seq
       (mapcat parse-schedule-line)))

(defn parse-schedule-file [file-path]
  (->> (jio/reader file-path)
       (parse-schedule-stream)))

(defn day-file-stream [year day]
  (jio/reader (jio/resource (str "bristol/" year "/csv/tabula-" (string/lower-case day) ".csv"))))

(defn read-schedules-dates [year]
  (into {} (map (fn [day] [(keyword (string/lower-case day))
                           (parse-schedule-file (day-file-stream year day))])
                ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday"])))

(defn waste-type-cycles [waste-cycle-type]
  (let [order (case waste-cycle-type
                :day-a [#{:blue} #{:green}]
                :day-b [#{:green} #{:blue}]
                :day-w [#{:blue}]
                :default (throw (ex-info "Unrecognised waste type" {:waste-type waste-cycle-type}) ))]
    (cycle order)))

(defn create-pickup [date] {:date date})

(defn add-waste-types [pickup waste-types]
  (update pickup :waste-types #(into waste-types %)))

(defn add-cyclical-waste [pickups waste-types]
  (map add-waste-types pickups waste-types))

(defn add-matching-date-waste [pickup date-to-waste]
  (if-let [waste-types (date-to-waste (:date pickup))]
    (add-waste-types pickup waste-types)
    pickup))

(defn add-date-waste [pickups date-to-waste]
  (map #(add-matching-date-waste % date-to-waste) pickups))

(defn add-xmas-tree [pickups date]
  (if date
    (add-date-waste pickups {date #{:xmas-tree}})
    pickups))

(defn build-schedule [schema lookup-schedule-dates lookup-waste-cycle]
  (with-meta
    {:pickups (-> (map create-pickup (lookup-schedule-dates (:day schema)))
                  (add-cyclical-waste (lookup-waste-cycle (:cycle schema)))
                  (add-xmas-tree (:xmas-tree schema)))}
    {:schema schema}))

(defn build-schedules [schemas lookup-schedule-dates lookup-waste-cycle]
  (map #(build-schedule % lookup-schedule-dates lookup-waste-cycle) schemas))

(def schedule-schemas
  {2019 [{:area "bristol" :ref "MON/A" :day :monday    :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-14") :url "http://www.bristolwastecompany.co.uk/media/filer_public/52/3c/523cd7dd-296b-4eca-a08d-cf56663b4c76/mona19.pdf"}
         {:area "bristol" :ref "MON/B" :day :monday    :cycle :day-b :xmas-tree (LocalDate/parse "2019-01-08") :url "http://www.bristolwastecompany.co.uk/media/filer_public/73/e9/73e9e555-85fe-45df-b531-bded3fa1bc84/monb19.pdf"}
         {:area "bristol" :ref "MON/W" :day :monday    :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-14") :url "http://www.bristolwastecompany.co.uk/media/filer_public/6a/3a/6a3ae231-6771-4088-be86-20f722485d1a/monw19.pdf"}
         {:area "bristol" :ref "TUE/A" :day :tuesday   :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-15") :url "http://www.bristolwastecompany.co.uk/media/filer_public/e8/65/e8659c9f-b0e3-4c64-81ea-304671071db9/tuesa19.pdf"}
         {:area "bristol" :ref "TUE/B" :day :tuesday   :cycle :day-b :xmas-tree (LocalDate/parse "2019-01-09") :url "http://www.bristolwastecompany.co.uk/media/filer_public/1e/06/1e0635c4-9850-4833-8c66-faf31770f86e/tuesb19.pdf" }
         {:area "bristol" :ref "TUE/W" :day :tuesday   :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-15") :url "http://www.bristolwastecompany.co.uk/media/filer_public/22/0d/220d2359-bb95-4285-be7f-740009697889/tuesw19.pdf"}
         {:area "bristol" :ref "WED/A" :day :wednesday :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-16") :url "http://www.bristolwastecompany.co.uk/media/filer_public/a7/74/a77411ce-354e-4d91-b01f-743d635bfe15/weda19.pdf"}
         {:area "bristol" :ref "WED/B" :day :wednesday :cycle :day-b :xmas-tree (LocalDate/parse "2019-01-10") :url "http://www.bristolwastecompany.co.uk/media/filer_public/eb/cf/ebcff438-0654-481e-bc04-a6d62db066b2/wedb19.pdf"}
         {:area "bristol" :ref "WED/W" :day :wednesday :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-16") :url "http://www.bristolwastecompany.co.uk/media/filer_public/99/5a/995ae3ce-ed23-491b-857f-f1829f2a27fa/wedw19.pdf"}
         {:area "bristol" :ref "THU/A" :day :thursday  :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-17") :url "http://www.bristolwastecompany.co.uk/media/filer_public/e8/a2/e8a24b84-bde7-4631-840d-67384e781bb6/thursa19.pdf"}
         {:area "bristol" :ref "THU/B" :day :thursday  :cycle :day-b :xmas-tree (LocalDate/parse "2019-01-11") :url "http://www.bristolwastecompany.co.uk/media/filer_public/b1/88/b188b72d-2af7-427e-981e-9b3d7ba83cc0/thursb19.pdf"}
         {:area "bristol" :ref "THU/W" :day :thursday  :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-17") :url "http://www.bristolwastecompany.co.uk/media/filer_public/36/17/361779e7-b7a9-4e34-88ef-8620e078083d/thursw19.pdf"}
         {:area "bristol" :ref "FRI/A" :day :friday    :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-18") :url "http://www.bristolwastecompany.co.uk/media/filer_public/ff/fd/fffdc487-1e86-4fb8-b414-0678a00009f5/fria19.pdf"}
         {:area "bristol" :ref "FRI/B" :day :friday    :cycle :day-b :xmas-tree (LocalDate/parse "2019-01-12") :url "http://www.bristolwastecompany.co.uk/media/filer_public/99/8f/998f6a7e-f145-4c1e-8601-8f92960c88e0/frib19.pdf"}
         {:area "bristol" :ref "FRI/W" :day :friday    :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-18") :url "http://www.bristolwastecompany.co.uk/media/filer_public/9f/eb/9febf236-f8e1-40d2-aa8f-b4cd3ac0feb4/friw19.pdf"}]})

(defn read-schedules [year]
  (build-schedules (schedule-schemas year) (read-schedules-dates year) waste-type-cycles))

(comment
  (:thursday-w (build-schedules schedule-schemas (read-schedules-dates 2019)))
  (:monday (read-schedules-dates 2019))
  (take 10 (cycle-types :b))
  (split-fields ["December ‘19,5th,12th,19th,,"])
  (:monday-b (read-schedules))
  )
