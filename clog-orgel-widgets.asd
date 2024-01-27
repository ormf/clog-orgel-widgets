;;;; clog-dsp-widgets.asd
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:clog-orgel-widgets
  :description "widgets for audio dsp guis in clog."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :depends-on (#:clog #:uuid)
  :serial t
  :components ((:file "package")
               (:file "globals")
               (:file "clog-redefs")
               (:file "classes")
               (:file "clog-orgel-widgets")
               (:file "init")))
