
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BACKTRACE; Base: 10 -*-
    
;;; Copyright (c) 2012-2013, Warren Wilkinson.  All rights reserved.

;;; BEGIN_LICENSE:LGPL2
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Library General Public License as published by
;;; the Free Software Foundation; version 2.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public License
;;; along with this library; see the file COPYING.LIB.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; END_LICENSE 
  
(defpackage :backtrace 
  (:use :common-lisp)
  (:export print-variable with-printable-backtrace backtrace))
  
(in-package :backtrace)
  
(defmacro aif (val then else)
  `(let ((it ,val)) (if it ,then ,else)))
  
(defun file-line (file top-level-form-number)
  (with-open-file (s file)
    (dotimes (i top-level-form-number (sb-impl::flush-whitespace s))
      (read s))
    (let* ((position (file-position s))
           (upto-start (make-string position)))
      (file-position s 0)
      (read-sequence upto-start s)
      (1+ (count #\Newline upto-start)))))

(defun source-location (frame)
  (let* ((loc (sb-di:frame-code-location frame))
         (dsource (sb-di:code-location-debug-source loc)))
    (aif (sb-di:debug-source-namestring dsource)
         (let ((truename (ignore-errors (truename it))))
           (if truename
               (concatenate 'string (namestring truename)
                            "@" 
                            (or (ignore-errors 
                                  (princ-to-string
                                   (file-line truename 
                                              (sb-di::code-location-toplevel-form-offset 
                                               (sb-debug::maybe-block-start-location loc)))))
                                "?"))
               it))
         (or (ignore-errors (sb-debug::code-location-source-form loc 100))
             "REPL, unknown location"))))

(flet ((frame-vars (frame) ;; adapted from swank
         (ignore-errors (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame))))
       (debug-var-value (var frame location) ;; adapted from swank
         (ecase (sb-di:debug-var-validity var location)
           (:valid (sb-di:debug-var-value var frame))
           ((:invalid :unknown) ':<not-available>))))
  (defun frame-variables (frame) ;; adapted from swark
    (let ((loc (sb-di:frame-code-location frame))
          (vars (frame-vars frame)))
      (when vars
        (loop for v across vars collect
             (list (sb-di:debug-var-symbol v) (debug-var-value v frame loc)))))))

(defun call-signature (frame)
  (with-output-to-string (a)
    (sb-debug::print-frame-call frame a)))

(defun print-variable (stream arg colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream "~a = ~a" (first arg) (second arg)))

(defun print-backtrace (stream variables frames &aux (i 0))
  (dolist (frame frames)
    (format stream "~%~d. ~a~%   - SOURCE: ~s~{~%    (with) ~/backtrace:print-variable/~}"
            i
            (call-signature frame)
            (source-location frame)
            (and variables (frame-variables frame)))
    (incf i)))

(defmacro with-printable-backtrace ((&optional stream &key (depth 5) variables) &body body)
  (let ((backtrace (gensym)))
    `(let ((,backtrace nil))
       (restart-case 
           (handler-bind
               ((error (lambda (condition)
                         (declare (ignore condition))
                         (setf ,backtrace
                               (nthcdr 3 
                                 (loop for i from 0 upto (+ 2 ,depth)
                                       for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
                                       collect frame)))
                         nil)))
             ,@body)
         (backtrace ()
           :report (lambda (r) (format r "Print ~a-level backtrace to ~a with~:[out~;~] variables"
                               ,depth ',stream ,variables))
           (print-backtrace ,stream ,variables ,backtrace)
           nil)))))
