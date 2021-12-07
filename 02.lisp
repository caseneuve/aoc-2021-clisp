;; --- Day 2: Dive! ---

(load "aoc")

(defparameter +day2+ (mapcar #'aoc:str-split (uiop:read-file-lines "02.in")))

;;; Solutions
(defun solution (input &key (fwd 0) (dep 0) aim)
  (loop for (sdir sval) in input
        for dir = (read-from-string sdir)
        for val = (parse-integer sval)
        do (case dir
             (forward (incf fwd val) (when aim (incf dep (* aim val))))
             (up (if aim (decf aim val) (decf dep val)))
             (down (if aim (incf aim val) (incf dep val)))))
  (* fwd dep))

(aoc:solve
 (solution +day2+)
 (solution +day2+ :aim 0))

;; Tests
(use-package :lisp-unit)

(define-test day2
  (let ((input '(("forward" "5")
                 ("down" "5")
                 ("forward" "8")
                 ("up" "3")
                 ("down" "8")
                 ("forward" "2"))))
    (assert-equal 150 (solution input))
    (assert-equal 900 (solution input :aim 0))))

(run-tests '(day2))
