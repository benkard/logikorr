(defpackage :logikorr-ht
  (:use #:hunchentoot #:common-lisp #:json #:yaclml))

(in-package #:logikorr-ht)

(defstruct student
  (id -1 :type integer)
  (score #() :type vector)
  (last-name "" :type (or string null))
  (first-name "" :type (or string null)))

(defparameter *directory* "/Users/mulk/Dropbox/Projekte/Logikorr/")
(defparameter *in-locked-context-p* nil)

(defvar *students* nil)
(defvar *database* (merge-pathnames "blatt3.txt" *directory*))
(defvar *lock* (merge-pathnames #p".mulk-db-lock" *database*))

(setq *hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8))

(defmacro with-authentication (() &body body)
  `(call-with-authentication (lambda () ,@body)))

(defun call-with-authentication (thunk)
  (multiple-value-bind (name password)
      (authorization)
    (if (and (string= name "test") (string= password "test"))
        (funcall thunk)
        (progn
          (require-authorization "Logik-Ergebniseingabe")))))

(defmacro with-data-lock (() &body body)
  `(call-with-data-lock (lambda () ,@body)))

(defun call-with-data-lock (thunk)
  (cond (*in-locked-context-p*
         (funcall thunk))
        (t
         (let (lock)
           (unwind-protect
               (progn
                 (loop until (setq lock (open *lock* :direction :output :if-exists nil :if-does-not-exist :create)))
                 (let ((*in-locked-context-p* t)) (funcall thunk)))
             (progn (ignore-errors (delete-file *lock*))
                    (ignore-errors (close lock))))))))


(defun find-students ()
  (with-data-lock ()
    (setq
     *students*
     (with-open-file (in *database* :external-format :utf-8)
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
                                       :score (make-array (list (length score))
                                                          :initial-contents score
                                                          :adjustable t
                                                          :fill-pointer t)
                                       :last-name last-name
                                       :first-name first-name))
               and do (incf id))))))

(define-easy-handler (show-main-page :uri "/") ()
  (with-authentication ()
    (let ((students (find-students)))
      (ignore-errors (setf (header-out :content-type) "text/html; charset=UTF-8"))
      (with-yaclml-output-to-string
        (<:html
         (<:head
          (<:title "Logik I: Korrekturergebnisse")
          (<:script :type "text/javascript"
                    :src "js/prototype.js")
          (<:script :type "text/javascript"
                    :src "js/scriptaculous.js")
          (<:link :type "text/css" :rel "stylesheet" :href "style.css")
          (<:script :type "text/javascript" :src "http://yui.yahooapis.com/3.0.0/build/yui/yui-min.js")
          (<:script :type "text/javascript" :src "logikorr.js")
          (<:script :type "text/javascript" :src "logikorr-completion-data.js"))
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
                      (<:td (<:as-html first-name))))))))))))


(define-easy-handler (logikorr.js :uri "/logikorr.js") ()
  (handle-static-file (relpath "logikorr.js")))

(define-easy-handler (logikorr-completion-data.js :uri "/logikorr-completion-data.js") ()
  (with-authentication ()
    (ignore-errors (setf (header-out :content-type) "text/javascript; charset=UTF-8"))
    (format nil "~%autocompleteList = ~A"
            (json:encode-json-to-string (mapcar (lambda (x) (unsplit-name (student-first-name x) (student-last-name x)))
                                                (find-students))))))


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
  (merge-pathnames path (make-pathname :directory *directory*)))

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

(define-easy-handler (find-student :uri "/find-student") (name)
  (with-authentication ()
    (let ((student (find-student-by-name name)))
      (setf (header-out :content-type) "text/json; charset=UTF-8")
      (with-slots (id first-name last-name score) student
         (with-output-to-string (*standard-output*)
           (json:encode-json-plist
            (list :id id
                  :first-name first-name
                  :last-name last-name
                  :score score)))))))

(define-easy-handler (update-student-score :uri "/update-student-score")
    (id score-number score)
  (with-authentication ()
    (with-data-lock ()
      (let ((student (find-student-by-id (parse-integer id)))
            (index   (parse-integer score-number)))
        (loop while (<= (length (student-score student)) index)
              do (vector-push-extend 0 (student-score student)))
        (setf (elt (student-score student) index)
              (let ((*read-eval* nil))
                (let ((score (read-from-string score)))
                  (check-type score number)
                  score))))
      (write-database)))
  "\"OK\"")

(defun write-database ()
  (with-data-lock ()
    (unless *students*
      (return-from write-database))
    (with-open-file (out *database* :external-format :utf-8 :direction :output :if-exists :new-version #+(or) :supersede)
      (dolist (student *students*)
        (if (student-last-name student)
            (format out "~&~A, ~A" (student-last-name student) (student-first-name student))
            (format out "~&~A" (student-first-name student)))
        (format out "~&(~{~3S~^ ~} )" (coerce (student-score student) 'list))
        (format out "~%~%")))))

(defun start-logikorr ()
  (start (make-instance 'acceptor :port 8080)))
