(load "aoc")
(defparameter +in1+ (mapcar 'parse-integer (uiop:read-file-lines "01.in")))

;; Tools
(defun sum-seq> (&rest xs)
  (apply #'> (mapcar (lambda (s) (apply #'+ s)) xs)))

(defun reductor (&rest args)
  (destructuring-bind ((old acc) el) args
    (let ((new (append (rest old) el)))
      (list new (push (sum-seq> new old) acc)))))

(defun compare-elements (n &optional (seq +in1+))
  (count 't
         (cadr
          (reduce #'reductor
                  (subseq seq n)
                  :initial-value (list (subseq seq 0 n) '())
                  :key #'list))))

;;; Solutions
(aoc:solve
 (compare-elements 1)
 (compare-elements 3)
 )

;; Tests
(use-package  :lisp-unit)

(define-test day01
  (let ((input '(199 200 208 210 200 207 240 269 260 263)))
    (assert-equal 7 (compare-elements 1 input))
    (assert-equal 5 (compare-elements 3 input))))

(run-tests '(day01))
