
(in-package :backtrace.test)
(defvar *fail-p* t)
(defun tertiary () ;; Line 4
  (declare (optimize debug))
  (if *fail-p* (error "In last") 4))

(defun beta (a) ;; Line 8
  (declare (optimize debug))
  (* (tertiary) a))

(defun alpha (a b) ;; Line 12
  (declare (optimize debug))
    (dotimes (i 4) 
      (incf b (beta a))))
