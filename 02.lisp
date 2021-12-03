(defparameter +in+ (mapcar (lambda (s) (cl-ppcre:split "\\ " s)) (uiop:read-file-lines "02.in")))
(defparameter +directions+ (pairlis '("up" "down") '(- +)))

;;; Solutions
(defun solution (input &key (fwd 0) (dep 0) aim)
  (loop for (dir input-value) in input
        for val = (parse-integer input-value)
        for op = (cdr (assoc dir +directions+ :test #'string=))
        if (string= dir "forward") do
          (setf fwd (+ fwd val))
          (when aim (setf dep (+ dep (* aim val))))
        else do
          (if aim
              (setf aim (funcall op aim val))
              (setf dep (funcall op dep val))))
  (* fwd dep))

(solution +in+)                              ; => 1488669
(solution +in+ :aim 0)                       ; => 1176514794

;;; Tests
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(define-test day02
  (let ((input '(("forward" "5")
                 ("down" "5")
                 ("forward" "8")
                 ("up" "3")
                 ("down" "8")
                 ("forward" "2"))))
    (assert-equal 150 (solution input))
    (assert-equal 900 (solution input :aim 0))))

(run-tests '(day02))
