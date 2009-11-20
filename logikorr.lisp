(defpackage :logikorr-ht
  (:use #:hunchentoot #:common-lisp #+(or) #:cl-prevalence #:json #:yaclml
        #:split-sequence #:xml-emitter)
  (:import-from #:parenscript #:@))

(in-package #:logikorr-ht)

(defstruct student
  (id -1 :type integer)
  (score #() :type vector)
  (last-name "" :type (or string null))
  (first-name "" :type (or string null)))

#+(or)
(defun create-student-id ()
  (1+ (reduce #'max (find-students) :key #'student-id :initial-value -1)))

(setq *hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8))

(defun find-students ()
  (with-open-file (in "/Users/mulk/Dropbox/Projekte/Logikorr/blatt3.txt" :external-format :utf-8)
    (loop for line = (read-line in nil nil nil)
          with id = 0
          while line
          unless (equal (remove #\Space line) "")
            collect (let* ((name line)
                           (score-line (read-line in t))
                           (score (read-from-string score-line))
                           (comma (position #\, name))
                           (last-name (if comma (subseq name 0 comma) nil))
                           (first-name (if comma (subseq name (+ comma 2)) name)))
                      (make-student :id id
                                    :score (coerce score 'vector)
                                    :last-name last-name
                                    :first-name first-name))
            and do (incf id))))

(define-easy-handler (show-main-page :uri "/") ()
  (let ((students (find-students)))
    (ignore-errors (setf (header-out :content-type) "text/html; charset=UTF-8"))
    (with-yaclml-output-to-string
     ;;with-yaclml-stream *hunchentoot-stream*
      (<:html
       (<:head
        (<:title "Logik I: Korrekturergebnisse")
        (<:script :type "text/javascript"
                  :src "js/prototype.js")
        (<:script :type "text/javascript"
                  :src "js/scriptaculous.js")
        (<:link :type "text/css" :rel "stylesheet" :href "style.css")
        #+(or)
        (<:link :type "text/css"
                :rel "stylesheet"
                :href "http://yui.yahooapis.com/2.8.0r4/build/autocomplete/assets/skins/sam/autocomplete.css")
        #+(or)
        (<:script :type "text/javascript"
                  :src #+(or) "http://yui.yahooapis.com/2.8.0r4/build/yuiloader/yuiloader-min.js"
                       "http://yui.yahooapis.com/combo?2.8.0r4/build/yahoo-dom-event/yahoo-dom-event.js&2.8.0r4/build/animation/animation-min.js&2.8.0r4/build/connection/connection-min.js&2.8.0r4/build/datasource/datasource-min.js&2.8.0r4/build/autocomplete/autocomplete-min.js&2.8.0r4/build/dragdrop/dragdrop-min.js&2.8.0r4/build/logger/logger-min.js")
        (<:script :type "text/javascript" :src "http://yui.yahooapis.com/3.0.0/build/yui/yui-min.js")
        (<:script :type "text/javascript" :src "logikorr.js"))
       (<:body
        (<:h1 "Logik I: Korrekturergebnisse")
        (<:h2 "Neue Ergebnisse")
        (<:table :id "ergebnisse")
        (<:h2 "Bestehende Ergebnisse")
        (<:table
         (<:tr
          (<:th "ID") (<:th "Punkte") (<:th "Nachname") (<:th "Vorname"))
         (dolist (student students)
           (with-slots (id score last-name first-name) student
              (<:tr (<:td (<:as-html id))
                    (<:td (<:as-html score))
                    (<:td (<:as-html last-name))
                    (<:td (<:as-html first-name)))))))))))

(define-easy-handler (logikorr.js :uri "/logikorr.js") ()
  (ignore-errors
    (setf (header-out :content-type) "text/javascript; charset=UTF-8"))
  (concatenate
   'string
   (ps:ps
     (defvar loader)
     (defvar autocomplete-list)
     (defvar autocomplete-data)
     ;; YUI 2 for autocompletion.
     #+(or)
     (setq loader (ps:new ((ps:@ *YAHOO* util *y-u-i-loader)
                           (ps:create :require '("animation" "yahoo-dom-event" "autocomplete" "datasource")
                                      :load-optional t
                                      :on-success (lambda ())
                                      :timeout 10000
                                      :combine t))))
     ;;(defvar autocomplete-list (ps:new (@ *YAHOO* util *local-data-source) ))
     ;; YUI 3 for the rest.
     ((@ (*YUI*) use)
      "node-base" "io-base" "io-form" "io-queue"
      (lambda (y)
        (defun make-student-row ()
          (let* ((table ((@ document get-element-by-id) "ergebnisse"))
                 (num (length (@ table rows)))
                 (row ((@ table insert-row) num)))
            (let* ((cell ((@ row insert-cell) 0))
                   (input ((@ document create-element) "input"))
                   (completion ((@ document create-element) "div")))
              ((@ input set-attribute) "type" "text")
              ((@ completion set-attribute) "class" "autocomplete")
              ((@ cell append-child) input)
              ((@ cell append-child) completion)
              ;;((@ *YAHOO* widget *auto-complete) input completion autocomplete-data)
              #+(or)
              (ps:new ((@ *autocompleter *local)
                       input
                       completion
                       autocomplete-list
                       (ps:create "fullSearch" t)))
              (ps:new ((@ *autocompleter *local)
                       input
                       completion
                       autocomplete-list
                       (ps:create "fullSearch" t))))            
            (let* ((cell ((@ row insert-cell) 1))
                   (input ((@ document create-element) "input")))
              ((@ input set-attribute) "type" "text")
              ((@ input set-attribute) "maxlength" "3")
              ((@ input set-attribute) "size" "3")
              ((@ cell append-child) input))))

        ((@ y on) "domready" make-student-row))))
   (format nil "~%autocompleteList = ~A"
           (json:encode-json-to-string (mapcar (lambda (x) (unsplit-name (student-first-name x) (student-last-name x)))
                                               (find-students))))
   ;;(format nil "~%autocompleteData = new YAHOO.util.LocalDataSource(autocompleteList);")
   ))

(define-easy-handler (style.css :uri "/style.css") ()
  (setf (header-out :content-type) "text/css; charset=UTF-8")  
  "body {
  z-index: 0;
}

div.autocomplete {
  position:absolute;
  width:350px;
  background-color:white;
  border:1px solid #888;
  margin:0;
  padding:0;
  z-index: 100;
}
div.autocomplete ul {
  list-style-type:none;
  margin:0;
  padding:0;
}
div.autocomplete ul li.selected { background-color: #ffb;}
div.autocomplete ul li {
  list-style-type:none;
  display:block;
  margin:0;
  padding:2px;
  height:32px;
  cursor:pointer;
}")

(defun relpath (path)
  (merge-pathnames path (make-pathname :directory "/Users/mulk/Dropbox/Projekte/Logikorr")))

(define-easy-handler (s1.js :uri "/js/builder.js") ()
  (handle-static-file (relpath "js/builder.js")))

(define-easy-handler (s2.js :uri "/js/scriptaculous.js") ()
  (handle-static-file (relpath "js/scriptaculous.js")))

(define-easy-handler (s3.js :uri "/js/controls.js") ()
  (handle-static-file (relpath "js/controls.js")))

(define-easy-handler (s4.js :uri "/js/dragdrop.js") ()
  (handle-static-file (relpath "js/dragdrop.js")))

(define-easy-handler (s5.js :uri "/js/effects.js") ()
  (handle-static-file (relpath "js/effects.js")))

(define-easy-handler (s6.js :uri "/js/prototype.js") ()
  (handle-static-file (relpath "js/prototype.js")))

(define-easy-handler (scriptaculous.js :uri "/js/scriptaculous.js") ()
  (handle-static-file (relpath "js/scriptaculous.js")))

(define-easy-handler (s8.js :uri "/js/slider.js") ()
  (handle-static-file (relpath "js/slider.js")))

(define-easy-handler (s9.js :uri "/js/sound.js") ()
  (handle-static-file (relpath "js/sound.js")))

(define-easy-handler (s10.js :uri "/js/unittest.js") ()
  (handle-static-file (relpath "js/unittest.js")))

#+(or)
(define-easy-handler (save-student :uri "/save-student")
    (id score surname firstname)
  )

(defun unsplit-name (first last)
  (if last
      (format nil "~A, ~A" last first)
      first))

(defun find-student-by-name (name)
  (dolist (student (find-students))
    (when (string= name (unsplit-name (student-first-name student)
                                      (student-last-name student)))
      (return-from find-student-by-name student))))

(defun find-student-by-id (id)
  (find id (find-students) :key #'student-id))

(define-easy-handler (find-student :uri "/find-student")
    (name)
  (let ((student (find-student-by-name name)))
    (setf (header-out :content-type) "text/json; charset=UTF-8")
    (with-slots (id first-name last-name score) student
       (json:encode-json-plist
        (list :id id
              :first-name first-name
              :last-name last-name
              :score score)))))

(define-easy-handler (update-student-score :uri "/update-student-score")
    (id score-number score)
  (let ((student (find-student-by-id (parse-integer id))))
    (setf (elt (student-score student) (parse-integer score-number))
          (let ((*read-eval* nil))
            (read score)))))

(define-easy-handler (add-student-score :uri "/add-student-score")
    (id)
  (let ((student (find-student-by-id (parse-integer id))))
    (vector-push-extend 0 (student-score student))
    (1- (length (student-score student)))))

(defun start-hunchenkorr ()
  (start (make-instance 'acceptor :port 8080)))
