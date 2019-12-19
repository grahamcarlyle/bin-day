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
  {
    2019
    [{:area "bristol" :ref "MON/A" :day :monday    :cycle :day-a :xmas-tree (LocalDate/parse "2019-01-14") :url "http://www.bristolwastecompany.co.uk/media/filer_public/52/3c/523cd7dd-296b-4eca-a08d-cf56663b4c76/mona19.pdf"}
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
     {:area "bristol" :ref "FRI/W" :day :friday    :cycle :day-w :xmas-tree (LocalDate/parse "2019-01-18") :url "http://www.bristolwastecompany.co.uk/media/filer_public/9f/eb/9febf236-f8e1-40d2-aa8f-b4cd3ac0feb4/friw19.pdf"}]
    2020
    [{:area "bristol" :ref "MON/A" :day :monday    :cycle :day-a :xmas-tree (LocalDate/parse "2020-01-13") :url "https://www.bristolwastecompany.co.uk/media/filer_public/1c/b2/1cb28a74-5869-4a65-a18d-dbc5a455c582/mon_a.pdf"}
     {:area "bristol" :ref "MON/B" :day :monday    :cycle :day-b :xmas-tree (LocalDate/parse "2020-01-07") :url "https://www.bristolwastecompany.co.uk/media/filer_public/a6/4e/a64e9bcb-b317-44a0-8a8b-671cbfbc31a6/mon_b.pdf"}
     {:area "bristol" :ref "MON/W" :day :monday    :cycle :day-w :xmas-tree (LocalDate/parse "2020-01-13") :url "https://www.bristolwastecompany.co.uk/media/filer_public/82/9a/829a00ee-8c8c-46fa-a479-8c2ea539e814/mon_w.pdf"}
     {:area "bristol" :ref "TUE/A" :day :tuesday   :cycle :day-a :xmas-tree (LocalDate/parse "2020-01-14") :url "https://www.bristolwastecompany.co.uk/media/filer_public/b1/82/b182e52c-6f3c-447a-9756-9c3f966171d5/tues_a.pdf"}
     {:area "bristol" :ref "TUE/B" :day :tuesday   :cycle :day-b :xmas-tree (LocalDate/parse "2020-01-08") :url "https://www.bristolwastecompany.co.uk/media/filer_public/38/db/38db035e-f006-4b9c-b099-56d61b5e07c4/tues_b.pdf" }
     {:area "bristol" :ref "TUE/W" :day :tuesday   :cycle :day-w :xmas-tree (LocalDate/parse "2020-01-14") :url "https://www.bristolwastecompany.co.uk/media/filer_public/a8/ba/a8ba7382-3490-43ab-90b4-a6143ee08bcf/tues_w.pdf"}
     {:area "bristol" :ref "WED/A" :day :wednesday :cycle :day-a :xmas-tree (LocalDate/parse "2020-01-15") :url "https://www.bristolwastecompany.co.uk/media/filer_public/48/cb/48cba71c-eb89-4633-a686-4b4f9abe6182/wed_a.pdf"}
     {:area "bristol" :ref "WED/B" :day :wednesday :cycle :day-b :xmas-tree (LocalDate/parse "2020-01-09") :url "https://www.bristolwastecompany.co.uk/media/filer_public/26/41/2641b4f1-4ed8-4e2f-ad32-1a3edf983d95/wed_b.pdf"}
     {:area "bristol" :ref "WED/W" :day :wednesday :cycle :day-w :xmas-tree (LocalDate/parse "2020-01-15") :url "https://www.bristolwastecompany.co.uk/media/filer_public/6a/fc/6afc459a-218b-4ed6-b391-41ef6509fbfe/wed_w.pdf"}
     {:area "bristol" :ref "THU/A" :day :thursday  :cycle :day-a :xmas-tree (LocalDate/parse "2020-01-16") :url "https://www.bristolwastecompany.co.uk/media/filer_public/f3/ad/f3ad67f5-6d0d-4c8e-a063-91500b692a3c/thurs_a.pdf"}
     {:area "bristol" :ref "THU/B" :day :thursday  :cycle :day-b :xmas-tree (LocalDate/parse "2020-01-10") :url "https://www.bristolwastecompany.co.uk/media/filer_public/1f/0f/1f0f0273-a162-44a0-87c6-9e7f70f831a4/thurs_b.pdf"}
     {:area "bristol" :ref "THU/W" :day :thursday  :cycle :day-w :xmas-tree (LocalDate/parse "2020-01-16") :url "https://www.bristolwastecompany.co.uk/media/filer_public/7a/15/7a154ef1-f70c-4c97-a591-4a5ec2086eb6/thurs_w.pdf"}
     {:area "bristol" :ref "FRI/A" :day :friday    :cycle :day-a :xmas-tree (LocalDate/parse "2020-01-17") :url "https://www.bristolwastecompany.co.uk/media/filer_public/86/3e/863ecbad-47c4-485c-b1cc-4b1b467ada9b/fri_a.pdf"}
     {:area "bristol" :ref "FRI/B" :day :friday    :cycle :day-b :xmas-tree (LocalDate/parse "2020-01-11") :url "https://www.bristolwastecompany.co.uk/media/filer_public/2a/59/2a597832-4a0d-491c-a595-3a276ebd4319/fri_b.pdf"}
     {:area "bristol" :ref "FRI/W" :day :friday    :cycle :day-w :xmas-tree (LocalDate/parse "2020-01-17") :url "https://www.bristolwastecompany.co.uk/media/filer_public/cc/1b/cc1b3597-31a5-457d-905e-edc85150ee49/fri_w.pdf"}]
   })

(defn read-schedules [year]
  (build-schedules (schedule-schemas year) (read-schedules-dates year) waste-type-cycles))

(comment
  (:thursday-w (build-schedules schedule-schemas (read-schedules-dates 2019)))
  (:monday (read-schedules-dates 2019))
  (take 10 (cycle-types :b))
  (split-fields ["December ‘19,5th,12th,19th,,"])
  (:monday-b (read-schedules))
  )
