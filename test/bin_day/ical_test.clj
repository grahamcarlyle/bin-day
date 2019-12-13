(ns bin-day.ical-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as jio]
            [bin-day.ical :as ical])
  (:import (java.time LocalDate)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn uk-util-date-to-local-date [date]
  (.toLocalDate (.atZone (.toInstant date) ical/uk-zone-id)))

(deftest waste-type-text
  (testing "summary"
    (is (= (ical/summarise-waste #{:blue}) "Recycling and waste"))
    (is (= (ical/summarise-waste #{:green}) "Recycling"))
    (is (= (ical/summarise-waste #{:green, :xmas-tree}) "Recycling, Christmas tree"))
    (is (= (ical/summarise-waste #{:blue, :xmas-tree}) "Recycling and waste, Christmas tree")))
  (testing "description"
    (is (= (ical/describe-waste #{:blue}) "Green box, Black box, Brown food waste bin, Black wheelie bin"))
    (is (= (ical/describe-waste #{:green}) "Green box, Black box, Brown food waste bin"))
    (is (= (ical/describe-waste #{:green, :xmas-tree}) "Green box, Black box, Brown food waste bin, Christmas tree"))))

(deftest build-calendars
  (let [schedule (with-meta {:pickups [{:date (LocalDate/of 2019 5 23), :waste-types #{:green}}]}
                            {:schema {:area "bristol" :ref "TUE/B" :day :tuesday :cycle :day-a :url "https://example.com/"}})
        calendar (ical/build-calendar schedule)]
    (is (= (.getValue (first (.getNames calendar))) "Bristol bin schedule: TUE/B"))
    (is (= (.getValue (first (.getDescriptions calendar))) "Schedule PDF https://example.com/"))
    (is (= (map (fn [event]
                  {:start-date (uk-util-date-to-local-date (.getValue (.getDateStart event)))
                   :summary (.getValue (.getSummary event))
                   :description (.getValue (.getDescription event))
                   :uid (.getValue (.getUid event))})
                (.getEvents calendar))
           [{:start-date (LocalDate/of 2019 5 23)
             :summary "Recycling"
             :description "Green box, Black box, Brown food waste bin"
             :uid "2019-05-23-TUE/B-bristol-bin-day@grahamcarlyle.com"}]))))

(deftest bless-schedules
  (let [year 2019
        temp-dir (.toFile (Files/createTempDirectory "bin-day-ics-" (into-array FileAttribute [])))
        _ (ical/schedules-to-calendars year temp-dir)
        generated-ics-files (filter #(.isFile %) (file-seq temp-dir))]
    (dorun
      (map (fn [generated-ics-file]
             (let [generated-ics (slurp generated-ics-file)
                   ics-file-name (.getName generated-ics-file)
                   blessed-ics-resource (jio/resource (.getPath (jio/file (ical/ics-resources year) ics-file-name)))
                   blessed-ics (slurp blessed-ics-resource)]
               (is (= generated-ics blessed-ics) (str "mismatch for " ics-file-name))
               (jio/delete-file generated-ics-file)))
           generated-ics-files))
    (jio/delete-file temp-dir)))

;; TODO
;; better diff output for blessing
;; timezone of the calendar
