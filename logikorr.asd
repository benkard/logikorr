(defsystem logikorr
  :name "logikorr"
  :version "0.0.1"
  :maintainer ""
  :author "Matthias Benkard <code@matthias.benkard.de>"
  :licence ""
  :description "Ein einfaches Bewertungsaufnahmesystem"
  :depends-on (:parenscript :alexandria :yaclml :hunchentoot :json :split-sequence :xml-emitter)
  :components ((:file "logikorr")))
