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

(defun ensure-string (token)
  (if (stringp token) token (format nil "~S" token)))

(defun addpx (token)
  (if (numberp token) (format nil "~apx" token) token))

(defun hex->rgb (num)
  (list
   (ash (logand num #xff0000) -16)
   (ash (logand num #xff00) -8)
   (logand num #xff)))

(defun cols->jsarray (cols)
  "transform a list of hex value colors into a javascript string"
  (format nil "[~{'rgba(~{~a~^, ~}, 1.0)'~^, ~}]" (mapcar #'hex->rgb cols)))

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
          (slider (create-div container
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
                              :data-value value
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
                              (let ((val-string (attribute slider "data-value")))
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


(defun multi-slider (container &rest args
                     &key (num 8) (colors #("blue" "green" "red"))
                       (min 0) (max 1) (mapping :lin)
                       (clip-zero nil)
                       (thumb t)
                       (direction "up") val-change-cb
                      &allow-other-keys)
  (let* ((css (getf args :css))
         (data-colors (format nil "'[~{\"~a\"~^, ~}]'" (coerce (or (getf args :colors) colors) 'list)))
         (msl (create-div container
                          :class "multislider"
                          :css (append
                                `(:color "transparent"
                                  :border "none"
                                  :display "flex"
                                  :padding "0.5pt")
                                css
                                '(:background-color "transparent")
                                (unless (getf css :height) `(:height ,*default-vslider-height*))
                                (unless (getf css :width) `(:width ,(format nil "~apx" (* num 17)))))
                          :data-num-sliders num
                          :data-min min
                          :data-max max
                          :data-mapping mapping
                          :data-clip-zero clip-zero
                          :data-colors data-colors
                          :data-direction (or (getf args :direction) direction)))
         ;; (subclass (if (member direction '("up" "down") :test #'string=) "mvslider" "mhslider"))
         ;; (inner-border (if (member direction '("up" "down") :test #'string=) :border-left :border-top))
         (sliders (let ((res (make-array num))
                        ;; (colorlen (length colors))
                        )
                    (dotimes (i num)
                      (setf (aref res i)
                            (create-div msl :data-idx i)))
                    res)))
    (js-execute msl (format nil "multislider(~A, { \"thumb\": '~(~a~)'})" (jquery msl) (if thumb "true" "false")))
    (dotimes (i num)
      (let ((slider (aref sliders i)))
        (clog::set-event slider "valuechange"
                         (let ((i i))
                           (lambda (data)
                             (declare (ignore data))
                             (let ((val-string (attribute slider "data-value")))
                               (if val-change-cb (funcall val-change-cb i val-string slider))))))))
    (values msl sliders)))

(let ((direction "up"))
  (cond 
    ((member direction '("up" "down") :test #'string=) 'up)
    (t nil)))

(defun numbox (container &rest args
               &key (color "#3071A9")
                 (background-color "#fff")
                 (selected-foreground "black")
                 (selected-background "lightblue")
                 (value 0)
                 (size 10)
                 min max
                 label
                 label-style
                 slot
                 val-change-cb
                 &allow-other-keys)
  (declare (ignorable slot min max))
  (let* ((css (getf args :css))
         (numbox
           (create-form-element
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
                    :width ,(addpx (* size 5))
                    :height ,(addpx (* size 2)))
                  css)
            :data-min (or (getf args :min) "false")
            :data-max (or (getf args :max) "false")
            :label (if label (create-label container :content (ensure-string label) :css '(:margin-right 0) :style label-style)))))
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


(defun toggle (container &key (style "") (content "") (size 6)
                           (color "black")
                           (background "white")
                           (selected-foreground "black")
                           (selected-background "orange")
                           (value-off "0.0")
                           (value-on "1.0")
                           val-change-cb (toggle-content "")
                           slot)
  (declare (ignore slot))
  (let ((btn (create-button container :class "toggle"
                                   :style (format nil "~A width: ~A;height: ~Apx;font-size: ~Apt;color: ~A;background-color: ~A;"
                                                     style (* 5 size) (* 2 size) size color background)
                                   :content content
                                   :data-val "0.0")))
    (let ((str (format nil "toggle(~A, { \"colorOff\": '~(~a~)', \"backgroundOff\": '~(~a~)', \"labelOff\": '~(~a~)', \"valueOff\": '~(~a~)', \"colorOn\": '~(~a~)', \"backgroundOn\": '~(~a~)', \"labelOn\": '~(~a~)', \"valueOn\": '~(~a~)'})"
                            (jquery btn) color background content value-off selected-foreground selected-background toggle-content value-on)))
;;;      (break "~S" str)
      (js-execute btn str))
    (set-on-click
     btn
     (lambda (obj)
       (declare (ignore obj))
       (let ((new-val (if (equal (attribute btn "data-val") value-on)
                           value-off value-on)))
         (setf (attribute btn "data-val") new-val)         
         (if val-change-cb (funcall val-change-cb new-val btn)))))
;;     (set-on-mouse-down
;;      btn
;;      (lambda (obj event)
;;        (declare (ignore obj event))
;; ;;;       (setf mouse-down t)
;;        (if button-state
;;            (progn
;;              ;; (setf (style obj "color") color)
;;              ;; (setf (style obj "background-color") background)
;;              ;; (setf (text obj) content)
;;              )
;;            (progn
;;              ;; (setf (style obj "color") selected-foreground)
;;              ;; (setf (style obj "background-color") selected-background)
;;              ;; (if toggle-content (setf (text obj) toggle-content))
;;              ))))
    btn))

(deftype vu-type () '(member :led :bar))
(deftype vu-input-mode () '(member :db :lin))
(deftype vu-display-map () '(member :pd :lin :dblin))
(deftype vu-direction () '(member :up :down :left :right))

;;; (format nil "~(~a~)" :--vu-background)
(defun vumeter (container &rest args
                &key style (width 10) (height 126) background
                  (direction :up) (border "thin solid black")
                  (vu-type :led) (input-mode :db) (display-map :pd) led-colors (bar-color "'rgba(60,60,255,1.0)'")
                  (data-db -100) inner-padding inner-padding-bottom
                &allow-other-keys)
  (declare (ignorable style border)
           (type vu-direction direction)
           (type vu-type vu-type)
           (type vu-input-mode input-mode)
           (type vu-display-map display-map))
  (dolist (key '(:type :input-mode :display-map :led-colors :data-db :style :inner-padding :inner-padding-bottom)) (remf args key))
  (setf (getf args :direction) direction)
  (setf (getf args :border) border)
  (setf (getf args :width) (format nil "~a" (addpx (if (member direction '(:left :right)) height width))))
  (setf (getf args :height) (format nil "~a" (addpx (if (member direction '(:left :right)) width height))))
  (when background (setf (getf args :--vu-background) background))
;;;  (break (format nil "justify-content: center;~{~(~A~): \"~a\"~^; ~}" args))
  (let ((vu-container (create-div container :class "vumeter" :style (format nil "justify-content: center;background-color: var(--vu-background);~{~(~A~): ~a; ~}~@[~a~]" args style) :data-db data-db)))
    (js-execute vu-container (format nil "vumeter(~A, {\"boxCount\": 40, \"ledColors\": ~a, \"barColor\": ~a, \"vuType\": '~(~a~)',
                                          \"inputMode\": '~(~a~)', \"displayMap\": '~(~a~)', \"direction\": '~(~a~)',
                                          \"innerPadding\": '~(~a~)', \"innerPaddingBottom\": '~(~a~)'})"
                                  (jquery vu-container)
                                  (if led-colors (if (symbolp led-colors) (format nil "\"~(~a~)\"" led-colors) (cols->jsarray led-colors)) "false")
                                  bar-color
                                  vu-type input-mode display-map direction
                                  (or inner-padding "false")
                                  (or inner-padding-bottom "false")))))


#|
(break (format nil "vumeter(~A, {\"boxCount\": 40, \"ledColors\": ~a, \"barColor\": ~a, \"vuType\": '~a', \"inputMode\": '~a', \"displayMap\": '~a' })"
                   "CLOG"
                   (if led-colors (cols->jsarray led-colors) "false")
                                  bar-color
                                  (jquery vu-container)
                                  vu-type input-mode display-map))


(defun multi-vu (container &key (num 8) (width 80) (height 100) background (direction :up) (border "")
                           (inner-background "var(--vu-background)") (inner-border "") inner-padding inner-padding-bottom
                           led-colors style
                             val-change-cb)
  (declare (ignorable val-change-cb))
  (let* ((mvu-container (create-div container
                                    :style (format nil "color: transparent; background-color: ~a;border: ~a;width: ~Apx;height: ~Apx;display: flex;padding: 0pt;~@[~A~]" (or background "transparent") border width height style)))
         (vus (v-collect (n num) (vumeter
                                  mvu-container
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
    (values vus mvu-container)))
|#
