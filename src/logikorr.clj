(ns logikorr
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use compojure.http compojure.html)
  (:require [appengine-clj.datastore :as ds]
            [org.danlarkin.json :as json])
  (:import [com.google.appengine.api.datastore DatastoreServiceFactory Entity Query Query$FilterOperator Query$SortDirection KeyFactory EntityNotFoundException Key]
           [com.google.appengine.api.users UserServiceFactory]))

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
  [map]
  (let [datastore (DatastoreServiceFactory/getDatastoreService),
        entity (.get datastore #^Key (:key map))]
    (doseq [[key value] (dissoc map :kind :key)]
      (.setProperty entity (name key) value))
    (.put datastore entity)))

(def *static-directory* "/Users/mulk/Dropbox/Projekte/LogiCLJ/war")

(defn current-revision []
  (or (first (ds/find-all (doto (Query. "revision")
                            (.addSort "number" Query$SortDirection/DESCENDING))))
      (ds/create {:kind "revision" :number 0})))

(defn find-students []
  (ds/find-all (doto (Query. "student" (:key (current-revision))))))

(defn position [coll thing]
  (loop [i 0,
         coll2 coll]
    (if (= (first coll2) thing)
      i
      (if-let [coll3 (next coll2)]
        (recur (+ i 1) coll3)
        nil))))

(defn split-name [name]
  (let [n (position name \,)]
    (if n
      [(apply str (drop (+ n 2) name)) (apply str (take n name))]
      [name ""])))

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
       [:table#ergebnisse]
       [:h2 "Bestehende Ergebnisse"]
       [:table
        [:tr
         [:th "ID"] [:th "Punkte"] [:th "Nachname"] [:th "Vorname"]]
        (map
         (fn [student]
           (let [{id :key score :score
                  last-name :last-name first-name :first-name}
                 student]
             [:tr
              [:td (str id)]
              [:td (str score)]
              [:td last-name]
              [:td first-name]]))
         students)]]])))

(defn unsplit-name [first last]
  (if last
    (str last ", " first)
    first))

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
  (let [student (find-student-by-id id)
        score (:score student)
        num (Integer. score-number)]
    (ds-update (assoc student
                 :score (concat (take num score)
                                [(Float. new-score-value)]
                                (drop (+ 1 num) score)))))
  "\"OK\"")

(defn make-new-revision []
  (with-ds-transaction
    (let [current (current-revision)
          students (find-students)
          new (ds/create {:kind "revision"
                          :number (inc (:number current))})]
      (doseq [student students]
        (ds/create (assoc (dissoc student :key) :kind "student") (:key new)))
      (str (:number new)))))

(defroutes logikorr
  (GET "/" index)
  (GET "/favicon.ico" (do nil))
  (GET "/logikorr-completion-data.js" (compute-completion-data-js))
  (GET "/find-student" (find-student-json (:name params)))
  (GET "/update-student-score" (update-student-score (:id params) (:score-number params) (:score params)))
  (GET "/make-new-revision" (make-new-revision))
  (GET "/*"
    (or (serve-file *static-directory* (params :*)) :next))
  (ANY "/*" (page-not-found)))

(defservice logikorr)
