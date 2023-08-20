;;; 
;;; clog-dsp-widget-example.lisp
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

(ql:quickload :clog-dsp-widgets)

(asdf:defsystem #:clog-dsp-widget-example
  :description "example for the usage of widgets for audio dsp guis in clog."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :depends-on (#:clog #:clog-dsp-widgets)
  :serial t
  :components ())

(defpackage #:clog-dsp-widgets-example
  (:use #:cl #:clog #:clog-gui #:clog-dsp-widgets))

(in-package :clog-dsp-widgets-example)

(defparameter *my-vumeter* nil)

(defun on-new-window (body)
  (clog-dsp-widgets-initialize body)
  (setf (title (html-document body)) "Clog DSP Widget Example")
  (add-class body "w3-blue-grey") ;;; background color

  ;; When doing extensive setup of a page using connection cache
  ;; reduces rountrip traffic and speeds setup.
  (with-connection-cache (body)
    (let ((db -20)
          (gui-container (create-div body :css '(:display flex
                                                    :flex-wrap wrap
                                                    :margin-right 15px
                                                    :padding-bottom 30px))))

      (vumeter gui-container :css '(:margin 10px)
                             :data-db db
                             :display-map :pd
                             :led-colors :pd)
      (setf *my-vumeter* (vumeter gui-container :css '(:margin 10px)
                                                :data-db db
                                                :vu-type :bar
                                                :display-map :pd
                                                :led-colors :pd))
      (vumeter gui-container :css '(:margin 10px)
                             :data-db db
                             :display-map :db-lin
                             :led-colors :green)

      (vumeter gui-container :css '(:margin 10px)
                             :data-db db
                             :display-map :db-lin
                             :led-colors :blue)
;;;      (toggle gui-container)

      ;; (radio gui-container :css '(:margin 10px :height 17px)
      ;;                      :background-off '("#666" "#999")
      ;;                      :label-off (loop for n below 8 collect n)
      ;;                      :label-on (loop for n below 8 collect n)
      ;;                      :direction "right"
      ;;                      :val-change-cb (lambda (v obj)
      ;;                                       (declare (ignore obj))
      ;;                                       (format t "radio: ~a~%" v)))
      ;; (radio gui-container :css '(:margin 10px :width 17px :height 136px)
      ;;                            :background-off '("#666" "#999")
      ;;                      :label-off (loop for n below 8 collect n)
      ;;                      :label-on (loop for n below 8 collect n)
      ;;                      :direction "down"
      ;;                      :val-change-cb (lambda (v obj)
      ;;                                       (declare (ignore obj))
      ;;                                       (format t "radio: ~a~%" v)))
      ;; (radio gui-container :css '(:margin 10px :height 17px)
      ;;                      :background-off '("#666" "#999")
      ;;                      :label-off (loop for n below 8 collect n)
      ;;                      :label-on (loop for n below 8 collect n)
      ;;                      :direction "left"
      ;;                      :val-change-cb (lambda (v obj)
      ;;                                       (declare (ignore obj))
      ;;                                       (format t "radio: ~a~%" v)))
      ;; 
      ;; (radio gui-container :css '(:margin 10px :width 17px :height 136px)
      ;;                      :background-off '("#666" "#999")
      ;;                      :label-off (loop for n below 8 collect n)
      ;;                      :label-on (loop for n below 8 collect n)
      ;;                      :direction "up"
      ;;                      :val-change-cb (lambda (v obj)
      ;;                                       (declare (ignore obj))
      ;;                                       (format t "radio: ~a~%" v)))
      ;; 
      ;; (radio gui-container :css '(:height 40px :margin 10px)
      ;;        :background-off "transparent")
      ;; (toggle gui-container :css '(:margin 10px :height 17px)
      ;;                       :background "transparent"
      ;;                       :val-change-cb (lambda (x obj)
      ;;                                        (declare (ignore obj))
      ;;                                        (format t "toggle: ~a~%" x)))
      ;; (numbox gui-container :css '(:margin 10px :width 50px)
      ;;         :min 10 :max 100 :value 300
      ;;                       :val-change-cb (lambda (value obj) (declare (ignore obj)) (format t "numbox value: ~a~%" value)))
      ;; (vslider gui-container
      ;;          :css '(:height 100px :flex "0 0 auto"
      ;;                 :margin 10px)
      ;;          :value 0.70
      ;;          :background "transparent"
      ;;          :color "transparent"
      ;;          :slider-thumb-height 1
      ;;          :val-change-cb (lambda (val obj)
      ;;                           (format t "slider element ~S changed to ~a~%" obj val)))
      ;; (vslider gui-container :css '(:height 100px :flex "0 0 auto" :margin 10px :background "transparent" :--bar-color "lightblue")
      ;;                        :value 0.2 :thumb nil :mapping :log :clip-zero t)
      ;; (vslider gui-container :css '(:height 100px :flex "0 0 auto" :margin 10px :background "transparent" :--bar-color "lightblue")
      ;;                        :value 0.2 :thumb nil :mapping :log :clip-zero t :direction "down")
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue") :value 0.50)
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px) :value 0.50)
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "#555" :--thumb-color "orange") :value 0.10)
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue" :--bar-color "lightgreen" :--thumb-color "red") :value 0.90)
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue" :--bar-color "#555" :--thumb-color "red") :value 0.90 :direction "up"
      ;;                        :val-change-cb (lambda (val obj) (declare (ignore obj)) (format t "value changed: ~a~%" val)))
      ;; (vslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue" :--bar-color "#555" :--thumb-color "red") :value 0.90 :direction "down"
      ;;                              :val-change-cb (lambda (val obj) (declare (ignore obj)) (format t "value changed: ~a~%" val)))
      ;; (hslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue" :--bar-color "#555" :--thumb-color "red") :value 0.90 :direction "right"
      ;;                        :val-change-cb (lambda (val obj) (declare (ignore obj)) (format t "value changed: ~a~%" val)))
      ;; (hslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "lightblue" :--bar-color "#555" :--thumb-color "red") :value 0.90 :direction "left"
      ;;                        :val-change-cb (lambda (val obj) (declare (ignore obj)) (format t "value changed: ~a~%" val)))
      ;; 
      ;; (hslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "transparent" :--bar-color "lightblue")
      ;;                        :value 0.2 :thumb nil :mapping :log :clip-zero t)
      ;; (hslider gui-container :css '(:flex "0 0 auto" :margin 10px :background "transparent" :--bar-color "lightblue")
      ;;                        :value 0.2 :thumb nil :mapping :log :clip-zero t :direction "left")
      ;; (setf *my-slider* (multiple-value-list
      ;;                    (multi-slider gui-container :css '(:background "transparent" :margin 10px :width 144px :height 144px) :num 8
      ;;                                                :val-change-cb (lambda (idx val obj) (format t "valueChanged: ~a ~a ~a~%"
      ;;                                                                                        idx val obj)))))
      ;; (multi-slider gui-container :direction "right" :css `(:margin 10px :width 144px :height 144px) :num 16)
      ;; (multi-slider gui-container :direction "down" :css `(:margin 10px  :width 144px :height 144px) :num 16)
      ;; (multi-slider gui-container :direction "left" :css `(:margin 10px :width 144px :height 144px) :num 16)
      

      )))

;;; *my-slider*

(defun start-dsp-widgets-example ()
  "Start Orgel Gui."
  (initialize 'on-new-window
              :static-root (merge-pathnames "./www/" (asdf:system-source-directory :clog-dsp-widgets)))
  (open-browser))

(start-dsp-widgets-example)
