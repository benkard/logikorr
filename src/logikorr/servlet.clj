(ns logikorr.servlet
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use compojure.http compojure.html)
  (:require [appengine-clj.datastore :as ds]
            [org.danlarkin.json :as json])
  (:import [com.google.appengine.api.datastore DatastoreServiceFactory Entity Query Query$FilterOperator Query$SortDirection KeyFactory EntityNotFoundException Key]
           [com.google.appengine.api.users UserServiceFactory]
           [java.util Properties]
           [javax.mail Message Message$RecipientType MessagingException Session Transport]
           [javax.mail.internet AddressException InternetAddress MimeMessage MimeMultipart MimeUtility MimeBodyPart]))

(defmacro with-ds-transaction [& body]
  `(call-with-ds-transaction (fn [] ~@body)))

(defn call-with-ds-transaction [thunk]
  (let [ds (DatastoreServiceFactory/getDatastoreService)
        success (atom false)
        transaction (.beginTransaction ds)
        return-value (atom nil)]
    (try (do
           (swap! return-value (fn [_] (thunk)))
           (.commit transaction)
           (swap! success (fn [_] true)))
         (finally
          (when-not @success
            (.rollback transaction))))
    @return-value))

(defn ds-update
  "Update the corresponding entity from the supplied map in the data store."
  [& entity-maps]
  (let [datastore (DatastoreServiceFactory/getDatastoreService)]
    (.put datastore
          (map (fn [entity-map]
                 (let [entity (.get datastore #^Key (:key entity-map))]
                   (doseq [[key value] (dissoc entity-map :kind :key)]
                     (.setProperty entity (name key) value))
                   entity))
               entity-maps))))

(def *static-directory* "/Users/mulk/Dropbox/Projekte/Logikorr/war")

(defn current-revision []
  (or (first (ds/find-all (doto (Query. "revision")
                            (.addSort "number" Query$SortDirection/DESCENDING))))
      (ds/create {:kind "revision" :number 0})))

(defn find-students []
  (ds/find-all (doto (Query. "student" (:key (current-revision)))
                 (.addSort "last-name")
                 (.addSort "first-name"))))

(defn position [coll thing]
  (loop [i 0,
         coll2 coll]
    (if (= (first coll2) thing)
      i
      (if-let [coll3 (next coll2)]
        (recur (+ i 1) coll3)
        nil))))

(defn split-name [name]
  (let [[last first] (.split name ",")]
    [(and first (.trim first)) (and last (.trim last))]))

(defn find-student-by-name [name]
  (let [[first-name last-name] (split-name name)]
    (first (ds/find-all (doto (Query. "student" (:key (current-revision)))
                          (.addFilter "first-name" Query$FilterOperator/EQUAL first-name)
                          (.addFilter "last-name" Query$FilterOperator/EQUAL last-name))))))

(defn find-student-by-id [id]
  (ds/get (KeyFactory/stringToKey id)))

(defn index [request]
  (let [students (find-students)]
    (html
     [:html
      [:head
       [:title "Logik I: Korrekturergebnisse"]
       [:script {:type "text/javascript" :src "js/prototype.js"}]
       [:script {:type "text/javascript" :src "js/scriptaculous.js"}]
       [:link {:type "text/css" :rel "stylesheet" :href "style.css"}]
       [:script {:type "text/javascript" :src "http://yui.yahooapis.com/3.0.0/build/yui/yui-min.js"}]
       [:script {:type "text/javascript" :src "logikorr.js"}]
       [:script {:type "text/javascript" :src "logikorr-completion-data.js"}]]
      [:body
       [:h1 "Logik I: Korrekturergebnisse"]
       [:h2 "Neue Ergebnisse"]
       [:form [:button#make-revision {:type "button"} "Aktuelle Version sichern"]
        [:div#new-version-label {:style "display: inline; color: #070"}]]
       [:form [:button#send-mail {:type "button"} "Datei an die Korrektoren schicken"]
        [:div#mail-sent-label {:style "display: inline; color: #070"}]]
       [:table#ergebnisse]
       [:h2 "Bestehende Ergebnisse"]
       [:a {:href "/logik.txt"} "(Als Text anzeigen.)"]
       [:table.score-table {:border "1"}
        [:tr
         [:th "ID"] [:th "Punkte"] [:th "Nachname"] [:th "Vorname"]]
        (map
         (fn [student]
           (let [{id :key score :score
                  last-name :last-name first-name :first-name}
                 student]
             [:tr
              [:td (str id)]
              [:td (map (fn [num]
                          (html [:span {:style "min-width: 2em; display: inline-block;"} num]
                                " "))
                        score)]
              [:td last-name]
              [:td first-name]]))
         students)]
       [:h2 "Daten importieren"]
       [:form {:method "POST" :action "/import-score-file"}
        [:textarea {:name "file-data" :cols "80" :rows "20"}]
        [:br]
        [:input {:type "submit"}]]]])))

(defn unsplit-name [first last]
  (if first
    (str last ", " first)
    last))

(defn compute-completion-data-js []
  (str "autocompleteList = "
       (json/encode-to-str (map (fn [x]
                                  (unsplit-name (:first-name x) (:last-name x)))
                                (find-students)))
       ";"))

(defn find-student-json [name]
  (let [student (find-student-by-name name)]
    (json/encode-to-str {:id (KeyFactory/keyToString (:key student))
                         :score (seq (:score student))
                         :first-name (:first-name student)
                         :last-name (:last-name student)})))

(defn update-student-score [id score-number new-score-value]
  (with-ds-transaction
    (let [student (find-student-by-id id)
          score (:score student)
          num (Integer. score-number)]
      (ds-update (assoc student
                   :score (concat (take num score)
                                  [(binding [*read-eval* false]
                                     (read-string new-score-value))]
                                  (drop (+ 1 num) score)))))
    "\"OK\""))

(defn make-new-revision []
  (with-ds-transaction
    (let [current (current-revision)
          students (find-students)
          new (ds/create {:kind "revision"
                          :number (inc (:number current))})
          new-key (:key new)
          datastore (DatastoreServiceFactory/getDatastoreService)]
      (.put datastore
            (map (fn [student]
                   (doto (Entity. "student" #^Key new-key)
                     (.setProperty "first-name" (:first-name student))
                     (.setProperty "last-name" (:last-name student))
                     (.setProperty "score" (:score student))))
                 students))
      (str (:number new)))))

(defn call-with-authentication [thunk]
  (let [users (UserServiceFactory/getUserService)
        user (.getCurrentUser users)]
    (if (and user
             (not (empty? (ds/find-all
                           (doto (Query. "user")
                             (.addFilter "email" Query$FilterOperator/EQUAL (.getEmail user)))))))
      (thunk)
      (redirect-to (.createLoginURL users "/")))))

(defn import-score-file [data]
  (loop [lines (.split data "\n")]
    (let [line (first lines)]
      (when line
        (if (= (.trim line) "")
          (recur (rest lines))
          (let [name line
                score-string (second lines)
                score (binding [*read-eval* false] (read-string score-string))
                student (find-student-by-name name)
                [first-name last-name] (split-name name)]
            (if student
              (ds-update (assoc student :score score))
              (ds/create {:last-name last-name
                          :first-name first-name
                          :score score
                          :kind "student"}
                         (:key (current-revision))))
            (recur (rest (rest lines)))))))))

(defn encode-database-file []
  (with-out-str
    (let [students (find-students)]
      (doseq [student students]
        (printf "%s\n" (unsplit-name (:first-name student) (:last-name student)))
        (printf "(")
        (doseq [score (:score student)]
          (printf "%-3s " (str score)))
        (printf ")\n\n")))))

(defn send-mail []
  (let [props (Properties.)
        session (Session/getDefaultInstance props nil)
        message (MimeMessage. session)
        users (ds/find-all (Query. "user"))]
    (doseq [user users]
      (.addRecipient message
                     Message$RecipientType/TO
                     (InternetAddress. (:email user) (:email user))))
    (doto message
      (.setFrom (InternetAddress. "mulkiatsch@gmail.com" "Logikorr"))
      (.setSubject "Logik-Korrekturergebnisse")
      (.setContent
       (doto (MimeMultipart.)
         (.addBodyPart (doto (MimeBodyPart.)
                         (.setText "Anbei die jüngsten Korrekturergebnisse.

Automatisch erzeugte Grüße,
Logikorr")))
         (.addBodyPart (doto (MimeBodyPart.)
                         ;;(.setText (encode-database-file))
                         (.setContent (encode-database-file) "text/plain")
                         ;; (.setDataHandler (doto (DataSource.)
                         ;;                    ()))
                         (.setFileName "logik.txt")
                         (.setDisposition "attachment"))))))
    (Transport/send message)
    "\"OK\""))

(defmacro with-authentication [& body]
  `(call-with-authentication (fn [] ~@body)))

(defroutes logikorr
  (GET "/" (with-authentication (index request)))
  (GET "/favicon.ico" (do nil))
  (GET "/logikorr-completion-data.js" (with-authentication (compute-completion-data-js)))
  (GET "/find-student" (with-authentication (find-student-json (:name params))))
  (GET "/update-student-score" (with-authentication (update-student-score (:id params) (:score-number params) (:score params))))
  (GET "/make-new-revision" (with-authentication (make-new-revision)))
  (GET "/send-mail" (with-authentication (send-mail)))
  (GET "/logik.txt" [{:headers {"Content-Type" "text/plain"}}
                     (with-authentication
                       (encode-database-file))])
  (POST "/import-score-file" (with-authentication
                               (import-score-file (:file-data params))
                               (redirect-to "/")))
  (GET "/*"
    (or (serve-file *static-directory* (params :*)) :next))
  (ANY "/*" (page-not-found)))

(defservice logikorr)
