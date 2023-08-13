;;; 
;;; example.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(asdf:defsystem #:clog-dsp-widget-example
  :description "example for the usage of widgets for audio dsp guis in clog."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :depends-on (#:clog #:clog-dsp-widgets)
  :serial t
  :components ())

(defpackage #:clog-dsp-widgets-example
  (:use #:cl #:clog #:clog-gui clog-dsp-widgets))

(in-package :clog-dsp-widgets-example)

;;; (clog-gui-initialize)

(defun on-new-window (body)
  (let (connection-id)
    (clog-gui-initialize body)
    (setf connection-id (clog::connection-id body))
    (load-script (html-document body) "js/vumeter.js")
    (load-script (html-document body) "js/toggle.js")
    (setf (title (html-document body)) "Orgel Sliders")
    (add-class body "w3-blue-grey") ;;; background color
    (load-css (html-document body) "/css/w3.css")
    (load-css (html-document body) "./css/custom-gui-elems.css")
    (setf (gethash "orgel-gui" (gethash connection-id clog-connection::*connection-data*))
          orgel-gui)
    ;; When doing extensive setup of a page using connection cache
    ;; reduces rountrip traffic and speeds setup.
    (with-connection-cache (body)
      (let ((gui-container (create-div body :style "display: flex;overflow: auto;margin-right: 15px;padding-bottom:30px;")))
        (dotimes (i 10)
          (let ((orgel (aref (orgel-gui-orgeln orgel-gui) i))
                (global-orgel-ref (aref *curr-state* i)))
            (create-orgel-gui i gui-container orgel global-orgel-ref)))))))
