;;; 
;;; package.lisp
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

(defpackage #:clog-dsp-widgets
  (:use #:cl #:clog #:clog-gui)
  (:shadowing-import-from #:clog #:rotate)
  (:export #:clog-dsp-widgets-initialize
           #:vslider #:hslider
           #:multi-slider
           #:toggle #:button
           #:bang
           #:flash
           #:pulse-on
           #:pulse-off
           #:highlight
           #:radio
           #:vradio #:hradio
           #:numbox
           #:vumeter
           #:multi-vu
           #:get-clog-window
           #:with-lists
           #:pref-second
           #:action-cb
           #:val-change-cb))
