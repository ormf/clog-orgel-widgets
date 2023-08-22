;;; 
;;; classes.lisp
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

(in-package :clog)

(defgeneric idx-value (clog-obj idx)
  (:documentation "Get/Setf form element value.
   Form element values are not accessible through the Text property but
   instead through the value property."))

(defgeneric (setf idx-value) (value clog-obj idx)
  (:documentation "Set value VALUE for CLOG-FORM-ELEMENT"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-toggle (clog-button)()
  (:documentation "CLOG toggle Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-toggle       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-toggle (clog-obj
                           &rest args
                           &key content class style hidden html-id auto-place)
  (:documentation "Create a new CLOG-Toggle as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-toggle ((obj clog-obj)
                          &rest args
                          &key content class style hidden html-id (auto-place t)
                          &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag
                     :button
                     (args->attribute-plist args)
                     content)
                :clog-type  'clog-toggle
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-toggle))
  (attribute obj "data-val"))

(defmethod (setf value) (value (obj clog-toggle))
  (setf (attribute obj "data-val") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-radio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-radio (clog-div)()
  (:documentation "CLOG radio Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-radio       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-radio ((obj clog:clog-obj) &rest args
                            &key content class style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-radio
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-radio))
  (attribute obj "data-val"))

(defmethod (setf value) (value (obj clog-radio))
  (setf (attribute obj "data-val") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-slider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-slider (clog-div)()
  (:documentation "CLOG slider Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-slider       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-slider ((obj clog:clog-obj) &rest args
                            &key content class style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-slider
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-slider))
  (attribute obj "data-val"))

(defmethod (setf value) (value (obj clog-slider))
  (setf (attribute obj "data-val") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-multislider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-multislider (clog-div)
  ((sliders :accessor sliders :initform nil :initarg sliders))
  (:documentation "CLOG multislider Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-multislider       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-multislider ((obj clog:clog-obj) &rest args
                            &key content class sliders style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class sliders style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-multislider
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod idx-value ((obj clog-multislider) idx)
  (attribute (aref (sliders obj) idx) "data-val"))

(defmethod (setf idx-value) (value (obj clog-multislider) idx)
  (setf (attribute (aref (sliders obj) idx) "data-val") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-numbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-numbox (clog-form-element)()
  (:documentation "CLOG numbox Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-numbox       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-numbox ((obj clog:clog-obj) element-type
                          &rest args
                          &key name label class style hidden html-id (auto-place t)
                          &allow-other-keys)
  (declare (ignorable name class style hidden))
  (let* ((element (create-child
                   obj (format-html-tag
                        :input
                        (args->attribute-plist args))
                   :clog-type  'clog-numbox
                   :html-id    html-id
                   :auto-place auto-place)))
    (when label
      (label-for label element))
    element))


;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-numbox))
  (read-from-string (property obj "value")))

(defmethod (setf value) (value (obj clog-numbox))
  (setf (property obj "value") value))

(defmethod min-val ((obj clog-numbox))
  (read-from-string (attribute obj "data-min")))

(defmethod (setf min-val) (value (obj clog-numbox))
  (setf (attribute obj "data-min") value))

(defmethod max-val ((obj clog-numbox))
  (read-from-string (attribute obj "data-max")))

(defmethod (setf max-val) (value (obj clog-numbox))
  (setf (attribute obj "data-max") value))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-vumeter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-vumeter (clog-div)()
  (:documentation "CLOG vumeter Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-vumeter       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-vumeter ((obj clog:clog-obj) &rest args
                            &key content class style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-vumeter
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-vumeter))
  (attribute obj "data-val"))

(defmethod (setf value) (value (obj clog-vumeter))
  (setf (attribute obj "data-val") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-multivu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-multivu (clog-div)
  ((meters :accessor meters :initform nil))
  (:documentation "CLOG multivu Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-multivu       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-multivu ((obj clog:clog-obj) &rest args
                            &key content class style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-multivu
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod idx-value ((obj clog-multivu) idx)
  (attribute (aref (meters obj) idx) "data-val"))

(defmethod (setf idx-value) (value (obj clog-multivu) idx)
  (setf (attribute (aref (meters obj) idx) "data-val") value))

(export '(CREATE-TOGGLE
          CREATE-RADIO
          CREATE-SLIDER
          CREATE-MULTISLIDER
          CREATE-NUMBOX
          CREATE-VUMETER
          CREATE-MULTIVU
          SLIDERS
          METERS
          IDX-VALUE
          MIN-VAL
          MAX-VAL)
        'CLOG)

