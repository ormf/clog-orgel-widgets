;;; 
;;; clog-dsp-widgets.lisp
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

(in-package :clog-dsp-widgets)

(defun get-clog-window (elem)
  (window (gethash
           "clog-body"
           (gethash
            (clog::connection-id elem)
            clog-connection::*connection-data*))))

(defun get-clog-body (elem)
  (gethash
   "clog-body"
   (gethash
    (clog::connection-id elem)
    clog-connection::*connection-data*)))

(defun ensure-list (arg)
  (typecase arg
    (string (list arg))
    (vector (coerce arg 'list))
    (list arg)
    (t (list arg))))

(defmacro with-lists (tokens &rest body)
  "rebind all tokens to ensure they are lists."
  `(let ,(mapcar (lambda (tk) (list tk `(ensure-list ,tk))) tokens)
     ,@body))

;;; (ensure-list "")
;;; (ensure-list #(1 2 3))

(defun ensure-string (token)
  (if (stringp token) token (format nil "~S" token)))

(defun addpx (token)
  (if (numberp token) (format nil "~apx" token) token))

(defun hex->rgb (num)
  (list
   (ash (logand num #xff0000) -16)
   (ash (logand num #xff00) -8)
   (logand num #xff)))

(defun list->js-array (list)
  "transform a list of hex value colors into a javascript string"
  (format nil "[~{'~a'~^, ~}]" list))

;;; (list->js-array '("blue" "green" "red"))  "['blue', 'green', 'red']"

(defun colors->js-strings (colors)
  "transform a list of colors to valid js strings"
  (mapcar (lambda (color) (typecase color
                       (number (format nil "rgba(~{~a~^, ~}, 1.0)" (hex->rgb color)))
                       (symbol (format nil "~(~a~)" color))
                       (t color)))
          colors))

(defun colors->js-array (colors)
  (list->js-array
   (colors->js-strings (ensure-list colors))))

(defun any->js-array (colors)
  (list->js-array (ensure-list colors)))
#|
(defun hslider
    (container &key (value 0.0) (min 0.0) (max 100.0) (thumbcolor "black") (color "#3071A9")
                 (border-right-width 1) (background "#fff") width height
                 val-change-cb)
    "horizontal slider including behaviour."
  (let ((hsl
          (create-form-element
           container :range
           :class "hslider"
           :style (format nil "--border-right-width: ~Apx;
--slider-thumb: ~A;--slider-color: ~A;
--slider-background: ~A;min-width: 0;flex: 1 1 0;--slider-thumb-height: 100%;
--slider-thumb-width: 2px;height: ~A;~@[width: ~A;~]"
                          border-right-width thumbcolor color background
                          (or height "10px") width)
           :value (format nil "~a" value)
           :min (format nil "~a" min)
           :max (format nil "~a" max))))
    (if val-change-cb
        (set-on-input
         hsl
         (lambda (obj)
           (declare (ignore obj))
           (let ((val (value hsl)))
;;;                                       (format t "vsl~a: ~a~%" (1+ idx) val)
             (funcall val-change-cb val hsl)))))
    hsl))
|#

(defun read-value (str)
  (read-from-string (cl-ppcre:regex-replace " *\([0-9.]+\).*" str "\\\1")))

(defmacro slider-template (class slider-length slider-width)
  `(let* ((css (getf args :css))
          (slider (create-slider container
                              :class ,class
                              :css (append
                                    css
;;; as window.getComputedStyle doesn't seem to work reliably,we
;;; explicitely set some default values here instead of using the .css
;;; file.
                                    (unless (getf css :height) (if ,slider-length `(:height ,,slider-length)))
                                    (unless (getf css :width) (if ,slider-width `(:width ,,slider-width)))
                                    (unless (getf css :background) `(:background ,*default-slider-background-color*))
                                    (unless (getf css :--thumb-color) `(:--thumb-color ,*default-slider-thumb-color*))
                                    (unless (getf css :--bar-color) `(:--bar-color ,*default-slider-bar-color*)))
                              :data-val value
                              :data-clip-zero clip-zero
                              :data-min min
                              :data-max max
                              :data-mapping mapping
                              :data-direction direction)))
;;;     (break "~S" css)
     (js-execute slider (format nil "slider(~A, { \"thumb\": '~(~a~)'})" (jquery slider) (if thumb "true" "false")))
     (if val-change-cb
         (progn
           (clog::set-event slider "valuechange"
                            (lambda (data)
                              (declare (ignore data))
                              (let ((val-string (attribute slider "data-val")))
;;;                                (format t "clog-dsp-widgets::slider-template, value-change: ~a~%" val-string)
                                (funcall val-change-cb val-string slider))))))
     slider))

(defun vslider
    (container &rest args
     &key (value 0.0) (min 0.0) (max 1.0)
     (thumb t) (clip-zero nil) (direction "up") (mapping :lin)
       style val-change-cb
     &allow-other-keys)
  "vertical slider including behaviour."
  (declare (ignorable style val-change-cb))
  (slider-template "vslider" *default-vslider-height* *default-vslider-width*))

(defun hslider
    (container &rest args
     &key (value 0.0) (min 0.0) (max 1.0)
     (thumb t) (clip-zero nil) (direction "right") (mapping :lin)
       style val-change-cb
     &allow-other-keys)
  "vertical slider including behaviour."
  (declare (ignorable style val-change-cb))
  (slider-template "hslider" *default-vslider-width* *default-vslider-height*))

(defun mslider
    (container &rest args
     &key (value 0.0) (min 0.0) (max 1.0)
     (thumb t) (clip-zero nil) (direction "right") (mapping :lin)
       style val-change-cb
     &allow-other-keys)
  "vertical slider including behaviour."
  (declare (ignorable style val-change-cb))
  (let ((slider-class (getf args :slider-class)))
    (remf args :sliderclass)
    (slider-template slider-class nil nil)))

;;; (colors->js-array '("blue" "green" "red"))

(defun multi-slider (container &rest args
                     &key (num 8) (colors '("blue" "green" "red"))
                       (min 0) (max 1) (mapping :lin)
                       (clip-zero nil)
                       (thumb t)
                       (direction "up") val-change-cb
                      &allow-other-keys)
  (let* ((css (getf args :css))
         (data-colors (colors->js-array (or (getf args :colors) colors)))
         (msl (create-multislider container
                          :class "multislider"
                          :css (append
                                `(:color "transparent"
                                  :background "transparent"
                                  :border "none"
                                  :display "flex"
                                  :padding "0.5pt")
                                css
                                (unless (getf css :height) `(:height ,*default-vslider-height*))
                                (unless (getf css :width) `(:width ,(format nil "~apx" (* num 10)))))
                          :data-num-sliders num
                          :data-min min
                          :data-max max
                          :data-mapping mapping
                          :data-clip-zero clip-zero
                          :data-direction (or (getf args :direction) direction)))
         ;; (subclass (if (member direction '("up" "down") :test #'string=) "mvslider" "mhslider"))
         ;; (inner-border (if (member direction '("up" "down") :test #'string=) :border-left :border-top))
         (sliders (let ((res (make-array num))
                        ;; (colorlen (length colors))
                        )
                    (dotimes (i num)
                      (setf (aref res i)
                            (create-slider msl :data-idx i)))
                    res)))
    (js-execute msl (format nil "multislider(~A, { \"colors\": ~a, \"thumb\": '~(~a~)'})" (jquery msl) data-colors (if thumb "true" "false")))
    (setf (sliders msl) sliders)
    (if val-change-cb
        (dotimes (i num)
          (let ((slider (aref sliders i)))
            (clog::set-event slider "valuechange"
                             (let ((i i))
                               (lambda (data)
                                 (declare (ignore data))
                                 (let ((val-string (attribute slider "data-val")))
                                   (if val-change-cb (funcall val-change-cb i val-string slider))))))))
        (warn "no val-change-cb!"))
    msl))

(let ((direction "up"))
  (cond 
    ((member direction '("up" "down") :test #'string=) 'up)
    (t nil)))

(defun numbox (container &rest args
               &key (style "")
                 (color "#3071A9")
                 (background-color "#fff")
                 (selected-foreground "black")
                 (selected-background "lightblue")
                 (value 0)
                 (size 6)
                 min max
                 label
                 (label-style "")
                 slot
                 val-change-cb
                 &allow-other-keys)
  (declare (ignorable slot min max))
  (let* ((css (getf args :css))
         (width (getf css :width))
         (height (getf css :height))
         (numbox
           (create-numbox
            container :text
            :class "numbox"
            :value value
            :css (append
                  `(:--text-color ,color
                    :align center
                    :background-color ,background-color
                    :--textbox-selected-foreground ,selected-foreground
                    :--textbox-selected-background ,selected-background
                    :font-size ,(addpx size)
                    :width ,(or width (addpx (* size 5)))
                    :height ,(or height (addpx (* size 1.5))))
                  (progn
                    (dolist (prop '(:width :height :font-size)) (remf css prop))
                    css))
            :data-min (or (getf args :min) "false")
            :data-max (or (getf args :max) "false")
            :style style))
         (label (if label (create-label container :content (ensure-string label) :css '(:margin-right 0 :user-select "none") :style label-style))))
    (declare (ignorable label))
;;;    (clog::unbind-event-script numbox "onmousedown")
     (js-execute numbox (format nil "numbox(~A)" (jquery numbox)))
     (if val-change-cb
         (progn
           (clog::set-event numbox "valuechange"
                            (lambda (data)
                              (declare (ignore data))
                              (let ((val-string (attribute numbox "value")))
                                (funcall val-change-cb val-string numbox))))))    
    numbox))

(defmethod text-value ((obj clog-progress-bar))
  (property obj "value"))

(defmethod (setf text-value) (value (obj clog-progress-bar))
  (setf (property obj "value") value))

(defun pref-second (val-list)
  (or (second val-list) (first val-list)))

(defun toggle (container &rest args
               &key (style "") (size 10)
                 (text-color "black")
                 (background '("white" "orange"))
                 (label "")
                 (values '("0" "1"))
                 (value 0)
                 val-change-cb
               &allow-other-keys)
  (with-lists (text-color background label values)
    (let* ((css (getf args :css))
           (btn (create-toggle
                 container
                 :class "toggle"
                 :css (append
                       `(:align center
                         :color ,(first text-color)
                         :background ,(first background)
                         :--textbox-selected-foreground ,(pref-second text-color)
                         :--textbox-selected-background ,(pref-second background)
                         :font-size ,(addpx size)
                         :width ,(or (getf css :width) (addpx (* size 5)))
                         :height ,(or (getf css :height) (addpx (* size 1.7))))
                       (progn
                         (dolist (prop '(:width :height)) (remf args prop))
                         css))
                 :data-val value
                 :style style)))
      (let ((str (format nil "toggle(~A, { ~{~{'~a': '~(~a~)'~}~^, ~} })"
                         (jquery btn)
                         `(("colorOff" ,(first text-color))
                           ("backgroundOff" ,(first background))
                           ("labelOff" ,(first label))
                           ("valueOff" ,(first values))
                           ("colorOn" ,(pref-second text-color))
                           ("backgroundOn" ,(pref-second background))
                           ("labelOn" ,(pref-second label))
                           ("valueOn" ,(pref-second values))))))
        ;;      (break "~S" str)
        (js-execute btn str))
      (if val-change-cb
          (progn
            (clog::set-event btn "valuechange"
                             (lambda (data)
                               (declare (ignore data))
                               (funcall val-change-cb (value btn) btn)))))
      btn)))

(defun button (container &rest args
               &key (style "") (size 10)
                 (text-color "black")
                 (background '("white" "orange"))
                 (label "")
                 val-change-cb
               &allow-other-keys)
  (with-lists (text-color background label)
    (let* ((css (getf args :css))
           (color (first text-color))
           (active-color (pref-second text-color))
           (bg (first background))
           (active-bg (pref-second background))
           (lbl (first label))
           (active-lbl (pref-second label))
           (btn (create-button
                 container
                 :class "bang"
                 :css (append
                       `(:align center
                         :color ,color
                         :margin ,(or (getf css :margin) "0px")
                         :background ,bg
                         :font-size ,(addpx size)
                         :width ,(or
                                  (getf css :width)
                                  (getf args :width)
                                  (addpx (* size 5)))
                         :height ,(or
                                   (getf css :height)
                                   (getf args :height)
                                   (addpx (* size 1.7))))
                       (progn
                         (dolist (prop '(:width :height)) (remf args prop))
                         css))
                 :content lbl
                 :style style)))
      (set-on-mouse-down
       btn
       (let ((color color) (bg bg) (lbl lbl)
             (active-color active-color) (active-bg active-bg)
             (active-lbl active-lbl))
         (lambda (obj evt)
           (declare (ignore obj evt))
           (setf (style btn "background") active-bg)
           (setf (style btn "color") active-color)
           (setf (inner-html btn) active-lbl)
           (if (functionp val-change-cb) (funcall val-change-cb))
           (sleep 0.1)
           (setf (style btn "background") bg)
           (setf (style btn "color") color)
           (setf (inner-html btn) lbl)
           )))
;;       (set-on-mouse-leave
;;        btn
;;        (let ((color color) (bg bg) (lbl lbl))
;;          (lambda (obj)
;;            (declare (ignore obj))
;;            (setf (style btn "background") bg)
;;            (setf (style btn "color") color)
;;            (setf (inner-html btn) lbl)
;;            )))
;;       (set-on-mouse-up
;;        btn
;;        (let ((color color) (bg bg) (lbl lbl))
;; ;;;         (declare (ignore lbl))
;;          (lambda (obj evt)
;;            (declare (ignore obj evt))
;; ;;;                   (format t "prv clicked!~%")
;;            (setf (style btn "background") bg)
;;            (setf (style btn "color") color)
;;            (setf (inner-html btn) lbl)
;;            (if (functionp val-change-cb) (funcall val-change-cb)))))
      btn)))

(defun radio (container &rest args
              &key (style "")
                (num 8)
                (text-color-off "black")
                (background-off "white")
                (label-off "")
                (text-color-on "black")
                (background-on "orange")
                (label-on "")
                (value 0)
                (direction "right")
                val-change-cb
              &allow-other-keys)
  (let* ((css (getf args :css))
         (radio (create-radio
                 container
                 :class "radio"
                 :css (append
                       `(:color "transparent"
                         :background "transparent"
                         :border "none"
                         :padding "0.5pt")
                       css
                       (unless (getf css :height) `(:height ,*default-vslider-height*))
                       (unless (getf css :width) `(:width ,(format nil "~apx" (* num 17)))))
                 :data-num num
                 :data-val value
                 :style style)))
    (let* ((str (format nil "radio(~A, { ~{~{'~a': ~(~a~)~}~^, ~} })"
                        (jquery radio)
                        `(("colorOff" ,(colors->js-array text-color-off))
                          ("backgroundOff" ,(colors->js-array background-off))
                          ("labelOff" ,(any->js-array label-off))
                          ("colorOn" ,(colors->js-array text-color-on))
                          ("backgroundOn" ,(colors->js-array background-on))
                          ("labelOn" ,(any->js-array label-on))
                          ("direction" ,(format nil "'~a'" direction))))))
      (js-execute radio str))
    (if val-change-cb
        (progn
          (clog::set-event radio "valuechange"
                           (lambda (data)
                             (declare (ignore data))
                             (funcall val-change-cb (value radio))))))
    radio))

(deftype vu-type () '(member :led :bar))
(deftype vu-input-mode () '(member :db :lin))
(deftype vu-display-map () '(member :pd :lin :db-lin))
(deftype vu-direction () '(member :up :down :left :right))

(defun js-string (string)
  (format nil "'~a'" string))

;;; (format nil "~(~a~)" :--vu-background)
(defun vumeter (container &rest args
                &key style (width 10) (height 126)
                  (direction :up) (border "thin solid black")
                  (vu-type :led) (input-mode :db) (display-map :pd) (led-colors :pd) (bar-color "rgba(60,60,255,1.0)")
                  (data-db -100) inner-padding inner-padding-bottom
                &allow-other-keys)
  (declare (ignorable style border)
           (type vu-direction direction)
           (type vu-type vu-type)
           (type vu-input-mode input-mode)
           (type vu-display-map display-map))
;;;  (format t "ledColors: ~a~%" led-colors)
  (when (getf (getf args :css) :background)
    (setf (getf (getf args :css) :--vu-background)
          (getf (getf args :css) :background)))
  (let* ((css (getf args :css))
         (vu-container (create-vumeter
                        container
                        :class "vumeter"
                        :css
                        (append
                         `(:justify-content "center"
                           :width ,(or (getf css :width) (addpx width))
                           :height,(or (getf css :height) (addpx height)))
                         css)
                        :direction (or (getf args :direction) "up")
                        :data-db data-db)))
    (let ((str (format nil "vumeter(~A, { ~{~{'~a': ~(~a~)~}~^, ~} })"
                       (jquery vu-container)
                       `(("boxCount" 40)
                         ("ledColors" ,(if led-colors
                                           (if (symbolp led-colors) (format nil "'~(~a~)'" led-colors)
                                               (colors->js-array led-colors))
                                           "false"))
                         ("barColor" ,(js-string bar-color))
                         ("vuType" ,(js-string vu-type))
                         ("inputMode" ,(js-string input-mode))
                         ("ledMapping" ,(js-string display-map))
                         ("direction" ,(js-string direction))
                         ("innerPadding" ,(if inner-padding (js-string inner-padding) "false"))
                         ("innerPaddingBottom" ,(if inner-padding-bottom (js-string inner-padding-bottom) "false"))))))
      (js-execute vu-container str)
      vu-container)))

(defun multi-vu (container &key (num 8) (width 80) (height 100)  (background "transparent") (direction :up) (border "")
                             (inner-background "var(--vu-background)") (inner-border "") inner-padding inner-padding-bottom
                             led-colors css
                             val-change-cb)
  (declare (ignorable val-change-cb))
  (let* ((mvu (create-multivu container
                              :css (append
                                    `(:color transparent
                                      :background-color ,background
                                      :border ,border
                                      :width ,width
                                      :height ,height
                                      :display flex
                                      :padding 0px)
                                    css)))
         (vus (loop
                for n below num
                collect (vumeter
                         mvu
                         :led-colors led-colors
                         :direction direction
                         :border inner-border
                         :data-db 0
                         :width "100%"
                         :height "100%"
                         :border-right-width (if (< n (1- num)) 0 1)
                         :background (or inner-background background "var(--vu-background)")
                         :inner-padding inner-padding
                         :inner-padding-bottom inner-padding-bottom))))
    (setf (meters mvu) (coerce vus 'vector))
    mvu))

