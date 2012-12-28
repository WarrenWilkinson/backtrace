
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BACKTRACE.TEST; Base: 10 -*-

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

(defpackage :backtrace.test
  (:use :common-lisp :backtrace)
  (:export run-tests))

(in-package :backtrace.test)

(defvar *all-tests* nil)
      
(defstruct results
  (tests 0)
  (failures nil))
(defun results-failure-count (results)
  (length (results-failures results)))
(defun results-successes (results)
  (- (results-tests results)
     (results-failure-count results)))

(defun runtest (fun results)
  (let* ((success t)
         (output (with-output-to-string (*standard-output*)
                   (setf success (funcall fun)))))
    (make-results
     :tests (1+ (results-tests results))
     :failures (if success
                   (results-failures results)
                   (acons fun output (results-failures results))))))

(defun present-failures (results)
  (format t "~%BACKTRACE FAILURES:~%")
  (loop for (fn . problems) in (results-failures results)
        do (format t "~%~a~a~%" fn problems)))
(defun present-results (results)
  (format t "~%BACKTRACE TEST RESULTS:")
  (format t "~%     Tests: ~a~%   Success: ~a~%  Failures: ~a" 
          (results-tests results)
          (results-successes results)
          (results-failure-count results))
  (when (results-failures results)
    (present-failures results)))
  
(defun run-tests ()
  (format t "~%RUNNING BACKTRACE TESTS...")
  (present-results 
   (reduce #'(lambda (test results) (runtest test results))
           *all-tests* :from-end t :initial-value (make-results))))       

(defun test-setup (depth vars)
   (with-output-to-string (s)
      (with-printable-backtrace (s :depth depth :variables vars)
          (alpha 4 5))))
 
(defun quick-test (depth vars)
   (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart (find-restart 'backtrace)))))
     (test-setup depth vars))) ;; Invoke restart

(defmacro deftest (name &key (vars nil) (depth 6) (regex "") (comment ""))
  `(progn 
     (defun ,name ()
         ,comment 
         (let ((backtrace (quick-test ,depth ,vars)))
            (or ,(if (char= #\! (char regex 0))
                     `(not (cl-ppcre:scan ,(subseq regex 1) backtrace))
                     `(cl-ppcre:scan ,regex backtrace))
                (prog1 nil (format t "Regex ~s did not match backtrace:~%~a" ,regex backtrace)))))
     (pushnew ',name *all-tests*)))

;; Generated tests
(deftest  variables-off :vars nil :depth  6 :regex                  "!with" :comment "When variables are disabled, we shouldn't see any.")
(deftest   variables-on :vars   t :depth  6 :regex                   "with" :comment "When enabled, we should see some.")
(deftest tertiary-first :vars nil :depth  1 :regex                  "!BETA" :comment "Tertiary should be first.")
(deftest    beta-second :vars nil :depth  2 :regex                 "!ALPHA" :comment "Beta should be second")
(deftest    alpha-third :vars nil :depth  3 :regex                  "ALPHA" :comment "Alpha should be second")
(deftest    tertiary-fp :vars nil :depth  1 :regex  "test-functions.lisp@4" :comment "Correct file location for tertiary")
(deftest        beta-fp :vars nil :depth  2 :regex  "test-functions.lisp@8" :comment "Correct file location for beta")
(deftest       alpha-fp :vars nil :depth  3 :regex "test-functions.lisp@12" :comment "Correct file location for alpha")

(defun ensure-restart-exists-test ()
  "Test that the backtrace restart is created."
  (block nil
    (handler-bind ((error (lambda (c) (declare (ignore c)) (return (find-restart 'backtrace)))))
      (test-setup 5 nil))))
(pushnew 'ensure-restart-exists-test *all-tests*)
