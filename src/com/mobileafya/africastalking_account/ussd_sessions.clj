; Copyright (C) 2021  Dennis Schridde
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
(ns com.mobileafya.africastalking-account.ussd-sessions
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]
    [clojure.data :as data]
    [clojure.data.csv :as csv]
    [clojure.data.json :as json]
    [clj-http.client :as client]
    [debux.core :refer [dbg]]
    [environ.core :refer [env]])
  (:import
    (java.util Currency Locale)
    (java.time Duration LocalDate LocalDateTime ZoneId ZonedDateTime)
    (java.time.format DateTimeFormatter)))

(defn select-vals [m ks]
  (remove nil?
          (reduce (fn [acc k]
                    (conj acc (m k)))
                  []
                  ks)))

(def africastalking-app-id (env :africastalking-app-id))
(def africastalking-account-email (env :africastalking-account-email))
(def africastalking-account-password (env :africastalking-account-password))

(def africastalking-timezone (ZoneId/of "UTC+3"))
(def africastalking-time-formatter (DateTimeFormatter/ofPattern "LLLL d, u h:m a" Locale/ENGLISH))

(def iso-date-formatter DateTimeFormatter/ISO_DATE)

(def no-currency (Currency/getInstance "XXX"))

(def columns [:Date :SessionId :ServiceCode :PhoneNumber :Hops :Duration :Cost :Status :Input])

(def africastalking-page-size 10000)

(defn same-or-throw [expected got]
  (let [[only-in-expected only-in-got same] (data/diff expected got)]
    (when (or (some? only-in-expected) (some? only-in-got))
      (throw (new Exception (str "As expected: " only-in-expected " Got extra: " only-in-got " Same: " same))))))

(defn csv-data->maps [csv-data]
  (some->> csv-data                                         ; Allow empty seqs, which signal the last page
           first                                            ; First row is the header
           (map #(string/replace % #"\s" ""))
           (map keyword)
           (same-or-throw columns))                         ; Ensure format is as expected
  (map zipmap
       (repeat columns)
       (rest csv-data)))

(defn parse-time [s]
  (ZonedDateTime/of
    (LocalDateTime/parse s africastalking-time-formatter)
    africastalking-timezone))

(defn parse-duration [s]
  (Duration/ofSeconds
    (Integer/parseInt (string/replace s #"s$" ""))))

(defn parse-cost [s]
  (if (= s "None")
    {:currency no-currency
     :value    0}
    (let [[^String currency-code ^String value] (string/split s #"\s+" 2)]
      {:currency (Currency/getInstance currency-code)
       :value    (BigDecimal. value)})))

(defn parse-ussd-input [s]
  (when (not= s "N/A")
    s))

(defn parse-session [session]
  (-> session
      (update :Date parse-time)
      (update :Duration parse-duration)
      (update :Cost parse-cost)
      (update :Hops #(Integer/parseInt %))
      (update :Status keyword)
      (update :Input parse-ussd-input)))

(defn format-session [session]
  (-> session
      (update :Date #(.format % DateTimeFormatter/ISO_INSTANT))
      (update :Duration #(.toString %))
      (update :Cost #(str (:currency %) " " (:value %)))
      (update :Hops str)
      (update :Status name)
      (update :Input #(or % ""))))

; Retrieve sessions between start and end date (both inclusive), up to a
; maximum of `africastalking-page-size`.  If this does not include all
; sessions in that range, Africa's Talking will give us the latest sessions
; (i.e. the ones closer to `end-date`).  If one day has more than
; `africastalking-page-size` sessions, we have no way of retrieving them all.
(defn get-sessions [^LocalDate start-date ^LocalDate end-date access-token]
  (->>
    (client/get
      (str
        "https://account.africastalking.com/api/v1/apps/"
        africastalking-app-id
        "/ussd/sessions/export")
      {:headers      {"X-Client-Id" "nest.account.dashboard"}
       :oauth-token  access-token
       :query-params {:page      0
                      :count     africastalking-page-size
                      :startDate (.format start-date iso-date-formatter)
                      :endDate   (.format end-date iso-date-formatter)}})
    :body
    csv/read-csv
    csv-data->maps
    (map parse-session)))

; Returns a lazy-seq of all sessions up to `end-date`
; WARNING: The lazy-seq will make HTTP calls!
(defn get-all-sessions
  ([access-token]
   (get-all-sessions (LocalDate/now) access-token))
  ([^LocalDate end-date access-token]
   (when-let [sessions (seq (get-sessions (.minusMonths end-date 1)
                                          end-date
                                          access-token))]
     (let [earliest-date (.toLocalDate
                           (:Date (last sessions)))]
       (lazy-seq (concat
                   sessions
                   (get-all-sessions
                     (.minusDays earliest-date 1)
                     access-token)))))))

(defn export-to
  ([filename access-token]
   (export-to filename (LocalDate/now) access-token))
  ([filename ^LocalDate end-date access-token]
   (let [sessions (->> (get-all-sessions end-date access-token)
                       (map format-session)
                       (map #(select-vals % columns)))]
     (with-open [writer (io/writer filename)]
       (csv/write-csv writer [(map name columns)])
       (csv/write-csv writer sessions)))))

(defn login [email password]
  (->
    (client/post
      "https://account.africastalking.com/api/v1/auth/signin"
      {:headers      {"X-Client-Id" "nest.account.dashboard"}
       :accept       :json
       :content-type :json
       :body         (json/write-str
                       {:email      email
                        :password   password
                        :timeOffset 60})})
    :body
    (json/read-str :key-fn keyword)
    :data
    :access_token))

(comment
  (let [token (login africastalking-username africastalking-password)]
    (export-to "sessions.csv" token)))
