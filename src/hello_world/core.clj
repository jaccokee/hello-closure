(ns hello-world.core
  (:gen-class)
  (:require [clojure.xml :as xml])
  (:import
    [java.io IOException ByteArrayInputStream]
    java.net.URI
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
          refund-cutoff-list (map (fn [{:keys [booking]}]
                                      (list user-id (:created-on booking))
                                      )
                                    bookings)
          ;bookings (map (assoc booking :refundCutoffDate refund-cutoff-list) bookings)
          ; map of meeting id to host
          meeting-host-map (get-hosts learner-meeting-list)]
      (map #(println (str "Oh happy day - " [created-on])) refund-cutoff-list)
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

  ; (defn get-other-attendees
  ;  "Retreive the other attendee for all slots
  ;   Return the results as a single map containing {:slot-id '(email-address user-id)} pairs."
  ;[emails slot-ids]
  ;(reduce-appointments emails (:bookings (json/parse-string (->
  ;                                                            (. InjectorSingleton getInjector)
  ;                                                            (.getInstance com.livemocha.ws.scholar.Schedule)
  ;                                                            (.getBookings slot-ids)
  ;                                                            (.toString))
  ;                                                          true))) )

  ;; Get upcoming appointments
  ;(defn get-upcoming-learner-appointments
  ;  " Retrieve all future bookings for the inferred user associated with join session links.
  ;Return a vector containing either a JSON object containing upcoming appointments at index 0
  ;or error information at index 1. "
  ;  [{user-id :x-livemocha-user :as headers} baseuri token]
  ;  (try
  ;    (let [url (str (:uri supersaas-config) " / " supersaas-agenda-path)
  ;          params (agenda-query user-id)
  ;          {:keys [status body]} (client/get url params)]
  ;      (cond
  ;        (= 200 status) [(to-attendee-response-json body user-id baseuri token) nil]
  ;        ;supersaas returns 404 when no results are found, but AEB wants empty result set response
  ;        (= 404 status) [{:results []} nil]
  ;        :default (do
  ;                   (log/error (format " error [% s] response from supersaas due to unexpected problem: % s " status body))
  ;                   [nil [500 " unknown " " Unknown error response encountered "]])))
  ;    (catch error/is-couch-error? e
  ;      ; generating the response for consistent logging
  ;      (error/generate-couch-error-response e)
  ;      [nil [500 " failed_request " " An internal request failed due to a couch exception "]])
  ;    (catch IOException e
  ;      (log/error " Failed to retrieve appointments from supersaas due to IOException " e)
  ;      [nil [500 " failed_request " " An internal request failed due to an IOException "]])
  ;    (catch Exception e
  ;      (log/error " Failed to retrieve appointments from supersaas due to Exception " e)
  ;      [nil [500 " failed_request " " An internal request failed due to an Exception "]])))

  (defn -main
    "Mimic what's in get-upcoming-learner-appointments."
    [& args]
    (let [body (get-xml)
          user-id "12345"
          baseuri "http://www.kee.com/"
          token "abcd"]
      (println (to-attendee-response-json body user-id baseuri token)))
    ;  (println (to-json (xml/parse (string-to-stream (get-xml)))))
    )