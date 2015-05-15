(ns hello-world.core
  (:gen-class)
  (:require [clojure.xml :as xml])
  (:import
    [java.io IOException ByteArrayInputStream]
    java.net.URI
    org.joda.time.DateTime
    org.joda.time.format.DateTimeFormat
    ))

(def attendee-type "attendee")
(def adobe-hostname "meet62145201.adobeconnect.com")

;; Utility
(defn string-to-stream
  "Wrap a string in an InputStream."
  [string]
  (ByteArrayInputStream.
    (.getBytes (.trim string))))


;; From host_util.clj in Scholar
(defn get-hosts
  "Return map of meeting ids to adobe hostname to support dynamic
  host assignment (retained because this is likely to happen again)."
  [user-meeting-pairs]
  (zipmap
    (map (fn [[user-id meeting-id]]
           meeting-id)
         user-meeting-pairs)
    (map (fn [[user-id meeting-id]]
           adobe-hostname)
         user-meeting-pairs)))

(defn calc-cutoff
  "for each 'start' date passed in, calculate the refund cutoff date and add it as ':refundCutoffDate'"
  [booking-startdate-pairs]
  (zipmap
    (map (fn [[booking-id start]]
           booking-id)
         booking-startdate-pairs)
    (map (fn [[booking-id start]]
           ; Must calc refundCutoffDate from start, replace start with that.
           ; Must also generate the map: {:booking-id xxx :refundCutoffDate yyy}
           start)
         booking-startdate-pairs)))

(defn get-xml
  "returns a sample XML response, like from SuperSaaS"
  []
  (let [xml-response "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <bookings>
      <booking id=\"19995706\">
        <full_name>AEB Learner One</full_name>
        <email>aeb_learner1@kee-inc.test</email>
        <created_by>210a2400-2a03-4d91-98f7-12150d5e5ab8</created_by>
        <updated_by></updated_by>
        <status_message>Paid with credit  </status_message>
        <deleted>false</deleted>
        <created_on type=\"datetime\">2015-03-09 22:15:28</created_on>
        <updated_on type=\"datetime\">2015-03-09 22:15:28</updated_on>
        <schedule id=\"174930\">Advanced English UK Group</schedule>
        <slot id=\"11061946\">
           <description>test slot</description>
           <finish>2015-05-15 05:00:00</finish>
           <location>B1</location>
           <start>2015-05-15 04:00:00</start>
           <title>Course 101</title>
        </slot>
      </booking>
    </bookings>"]
        (str xml-response)))

(defn add-links
  "Associate join session links for Adobe Connect with bookings when the start time is within
  the next hour."
  [access_token baseuri participant-type user-id meeting-host-map {:keys [booking] :as doc}]
  (let [meeting-id (:slot-id booking)
        host (get meeting-host-map meeting-id)]
    (assoc doc
      :joinLink (str baseuri "schedule/join/" participant-type "?meetingID=" meeting-id "&host=" host "&access_token=" access_token))))

(defn to-json
  "Convert supersaas XML response to a JSON object."
  [{:keys [tag attrs content] :as body}]
  ;(println (str body))
    ;(if (string? body) (println "string"))
    ;(if (vector? body) (println "vector"))
    ;(if (map? body) (println "map"))
    (cond
      (string? body) body
      (vector? body) (if (= 1 (count body))
                       (to-json (first body))
                       (map to-json body))
      (map? body) (cond
                    ; merge known object arrays into a single object
                    (or (= :slot tag) (= :booking tag))


                    {tag (reduce merge (to-json content)) (keyword (str (name tag) "-id")) (get attrs :id)}
                    ; store the schedule id from the schedule tag
                    (= :schedule tag)
                    {tag (to-json content) (keyword (str (name tag) "-id")) (get attrs :id)}
                    ; treat everything else as is
                    :else
                    {tag (to-json content)})))

  (defn to-attendee-response-json
    "Convert a list of attendee bookings into a JSON object to be returned."
    [body user-id baseuri token]
    (let [bookings (or (:bookings (to-json (xml/parse (string-to-stream body)))) [])
          bookings (if (sequential? bookings) bookings [bookings])
          learner-meeting-list (map (fn [{:keys [booking]}]
                                      (list user-id (:slot-id booking))
                                      )
                                    bookings)
          refund-cutoff-list (map (fn [{:keys [booking booking-id]}]
                                      (list booking-id (get-in booking [:slot :start]))
                                    )
                                    bookings)
          refund-cutoff-map (calc-cutoff refund-cutoff-list)
          bookings2 (map (fn [{:keys [booking]}] (merge booking refund-cutoff-map)) bookings)
          bookings3 (merge bookings refund-cutoff-map)
          ;refund-cutoff-list {:x 5 :y 7 :z 4 :u 10 :v 12 :w 1}

          ; map of meeting id to host
          meeting-host-map (get-hosts learner-meeting-list)]
      ; Debug stuff - jkee:
      ;(if (map? refund-cutoff-list) (println "refund-cutoff-list is a map"))
      ;(if (vector? refund-cutoff-list) (println "refund-cutoff-list is a vector"))
      ;(if (string? refund-cutoff-list) (println "refund-cutoff-list is a string"))
      ;(if (list? refund-cutoff-list) (println "refund-cutoff-list is a list"))
      ;(if (nil? refund-cutoff-list) (println "refund-cutoff-list is nil"))
      ;(if (seq? refund-cutoff-list) (println "refund-cutoff-list is a seq"))
      (println (str "bookings:\n " bookings) "\n")
      (println (str "bookings2:\n " bookings2) "\n")
      (println (str "bookings3:\n " bookings3) "\n")
      (println (str "refund-cutoff-map:\n " refund-cutoff-map) "\n")
      (doseq [item refund-cutoff-list] (prn (str "sequence item - " item)))

      {:results (map (partial add-links token baseuri attendee-type user-id meeting-host-map) bookings)}))


  ;(defn reduce-appointments
  ;  "Turn all appointments into {:slot-id '(email-address user-id)} pairs"
  ;  [emails bookings]
  ;  (reduce merge (map (fn [{:keys [booking]}]
  ;                       (when (not (some #{(:email booking)} emails))
  ;                         {(keyword (:slot-id booking)) (list
  ;                                                         (:email booking)
  ;                                                         (first
  ;                                                           (clojure.string/split
  ;                                                             (:created-by booking)
  ;                                                             #" / ")))}))
  ;                     bookings)))

(defn calc-date
  "given a date as a string, add (or subtract) num days and return the new date"
  [start-date num]
  (let [
         formatter (. DateTimeFormat forPattern "yyyy-MM-dd HH:mm:ss")
         formatter (. formatter withZoneUTC)
         newdate (. formatter parseDateTime start-date)
         newdate (. newdate plusDays num)
        ;(. String valueOf \c)   ; invoke the static method String.valueOf(char) with argument 'c'
       ]
    newdate)
  )

(defn -main
  "Mimic what's in get-upcoming-learner-appointments."
  [& args]
  (let [body (get-xml)
        user-id "12345"
        baseuri "http://www.kee.com/"
        token "abcd"
        newdate (calc-date "2015-05-15 04:00:00" -1)]
    (println (to-attendee-response-json body user-id baseuri token))
;    (prn (str ("\nrefund date: " newdate)))
    (println (str "\nrefund date: \n" newdate) "\n")
    )
  )
