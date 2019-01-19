(ns bin-day.schedule-test
  (:require [clojure.test :refer [deftest is testing]]
            [bin-day.schedule :as schedule])
  (:import (java.io StringReader BufferedReader)
           (java.time LocalDate)))

(defn str-rdr [s] (BufferedReader. (StringReader. s)))

(deftest parse-single-schedule-line
  (is (= (schedule/parse-schedule-line "February ‘02,26th")
         [(LocalDate/of 2002 02 26)]))
  (is (= (schedule/parse-schedule-line "December ‘19,7th,21st,")
         [(LocalDate/of 2019 12 7) (LocalDate/of 2019 12 21)])))

(deftest parse-multiple-schedule-lines
  (is (= (schedule/parse-schedule-stream (str-rdr "July ‘18,15th,
August ‘19,3rd,"))
         [(LocalDate/of 2018 7 15) (LocalDate/of 2019 8 03)])))

(deftest build-schedule
  (let [schema {:ref "MON/A", :day :monday, :cycle :day-a :xmas-tree (LocalDate/of 2019 5 23)}
        schedule (schedule/build-schedule schema
                                      {:monday [(LocalDate/of 2019 5 9)
                                                (LocalDate/of 2019 5 16)
                                                (LocalDate/of 2019 5 23)]}
                                      {:day-a (cycle [#{:blue}, #{:green}])})]
    (is (= schedule
           {:pickups [{:date (LocalDate/of 2019 5 9), :waste-types #{:blue}}
                      {:date (LocalDate/of 2019 5 16), :waste-types #{:green}}
                      {:date (LocalDate/of 2019 5 23), :waste-types #{:blue, :xmas-tree}}]}))
    (is (= (:schema (meta schedule)) schema))))

(deftest aspects-of-build-schedule
  (let [schedule-schema {:ref "MON/A", :day :monday, :cycle :day-a}
        date1 (LocalDate/of 2019 5 16)
        date2 (LocalDate/of 2019 5 23)
        date3 (LocalDate/of 2019 5 30)
        waste-cycles1 {:day-a (cycle [#{:blue}, #{:green}])}]

    (testing "pickup dates from matching day's schedule"
      (let [schedule (schedule/build-schedule (assoc schedule-schema :day :monday)
                                          {:monday [date1 date2 date3]}
                                          waste-cycles1)]
        (is (= (map :date (:pickups schedule))
               [date1, date2, date3]))))

    (testing "pickup waste type from matching waste cycle"
      (let [schedule (schedule/build-schedule (assoc schedule-schema :cycle :day-a :day :monday)
                                          {:monday [date1 date2 date3]}
                                          {:day-a (cycle [#{:blue}, #{:green}])})]
        (is (= (map :waste-types (:pickups schedule))
               [#{:blue}, #{:green}, #{:blue}]))))

    (testing "xmas tree pickup date"
      (let [schedule (schedule/build-schedule (assoc schedule-schema :xmas-tree date2 :day :monday)
                                          {:monday [date1 date2 date3]}
                                          waste-cycles1)]
        (is (= (map #(clojure.set/intersection #{:xmas-tree} (:waste-types %)) (:pickups schedule))
               [#{} #{:xmas-tree}, #{}]))))))

(deftest build-schedules
  (is (= (schedule/build-schedules
           [{:ref "MON/A", :day :monday, :cycle :day-a}
            {:ref "TUE/B", :day :tuesday, :cycle :day-b, :xmas-tree (LocalDate/of 2019 4 11)}]
           {:monday [(LocalDate/of 2019 5 23)], :tuesday [(LocalDate/of 2019 4 4) (LocalDate/of 2019 4 11)]}
           {:day-a (cycle [#{:blue}, #{:green}]) :day-b (cycle [#{:green}, #{:blue}])}
           )
         [{:pickups [{:date (LocalDate/of 2019 5 23), :waste-types #{:blue}}]}
          {:pickups [{:date (LocalDate/of 2019 4 4), :waste-types #{:green}}
                     {:date (LocalDate/of 2019 4 11), :waste-types #{:blue, :xmas-tree}}]}])))

