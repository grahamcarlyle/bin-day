(ns bin-day.ical
  (:require [bin-day.schedule :as schedule]
            [clojure.java.io :as jio]
            [clojure.string :as string])
  (:import [java.time ZoneId LocalDate]
           [biweekly ICalendar]
           (biweekly.component VEvent)
           (java.util Date)
           (java.io File)))

(def uk-zone-id (ZoneId/of "Europe/London"))

(defn local-date-to-uk-util-date [#^LocalDate local-date]
  (Date/from (.toInstant (.atStartOfDay local-date uk-zone-id))))

(defn summarise-waste [waste-types]
  (str
    (if (:blue waste-types) "Recycling and waste")
    (if (:green waste-types) "Recycling")
    (if (:xmas-tree waste-types) ", Christmas tree")))

(defn describe-waste [waste-types]
  (str
    "Green box, Black box, Brown food waste bin"
    (if (:blue waste-types) ", Black wheelie bin")
    (if (:xmas-tree waste-types) ", Christmas tree")))

(defn stable-uid ^String [pickup schema]
  (str (:date pickup) "-" (:ref schema) "-" (:area schema) "-bin-day@grahamcarlyle.com"))

(defn to-event [pickup schema]
  (let [NIL! nil]
    (doto (VEvent.)
      (.setDateStart (local-date-to-uk-util-date (:date pickup)) false)
      (.setSummary #^String (summarise-waste (:waste-types pickup)))
      (.setDescription #^String (describe-waste (:waste-types pickup)))
      (.setTransparency true)
      (.setDateTimeStamp #^Date NIL!)
      (.setUid (stable-uid pickup schema)))))

(defn build-calendar ^ICalendar [schedule]
  (let [calendar (ICalendar.)
        schema (:schema (meta schedule))
        events (map #(to-event % schema) (:pickups schedule))]
    (doto calendar
      (.setProductId "https://github.com/grahamcarlyle/bin-day")
      (.setName (str "Bristol bin schedule: " (:ref schema)))
      (.setDescription (str "Schedule PDF " (:url schema))))
    (dorun (map #(.addEvent calendar %) events))
    calendar))

(defn ics-file-name [schema]
  (let [cleaned-ref (-> (:ref schema) (string/lower-case) (string/replace "/" "_"))]
    (str cleaned-ref ".ics")))

(defn write-calendars [schedules dest-dir]
  (dorun
    (map
      (fn [schedule]
        (let [schema (:schema (meta schedule))
              filepath (jio/file dest-dir (ics-file-name schema))
              calendar (build-calendar schedule)]
          (spit filepath (.write calendar))))
      schedules)))

(defn ics-resources [year] (str "bristol/" year "/ics"))

(defn schedules-to-calendars
  ([year] (schedules-to-calendars (File. (.toURI (jio/resource (ics-resources year))))))
  ([year dest-dir] (write-calendars (schedule/read-schedules year) dest-dir)))

(comment
  (build-calendar (:monday-b (schedule/read-schedules)))
  (schedules-to-calendars 2019)
  )
